--  Copyright (c) University of Florida
--
--  This file is part of window_gen.
--
--  window_gen is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  window_gen is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with window_gen.  If not, see <http://www.gnu.org/licenses/>.

-- Eric Schwartz
-- Greg Stitt
-- University of Florida

-- Description: This entity provides a testbench for the window generator with
-- a variety of simulation parameters. See the generic descriptions below.

-- TODO: Add a worst-case timeout in case the done signal is never received.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.math_custom.all;
use work.window_gen_pkg.all;
use work.tb_pkg.all;

-------------------------------------------------------------------------------
-- Generics Description
--
-- (WIDOW GENERATOR CONFIGRATION OPTIONS)
-- PARALLEL_IO :  Specifies the number of inputs read at a time, in addition to
--                the number of windows generated in parallel. The number of
--                inputs and outputs has to be the same. It is not possible
--                to generate more outputs than the number of provided inputs.
--                If more inputs can be provided each cycle than the desired
--                number of parallel window outputs, use a FIFO to stall the
--                input stream.
-- MAX_WINDOW_ROWS : The maximum number of rows in a generated window
-- MAX_WINDOW_COLS : The maximum number of cols in a generated window
-- MAX_IMAGE_ROWS : The maximum number of rows in an input image/stream
-- MAX_IMAGE_COLS : The maximum number of cols in an input image/stream
-- DATA_WIDTH : The width in bits of each element of the input stream.
-- INPUT0_AT_MSB : Parallel inputs can be delivered with the first
--                 input (INPUT0) at the MSB or the first input stored at
--                 the LSB. e.g., for PARALLEL_IO = 4:
--                 in0 & in1 & in2 & in3 (for INPUT0_AT_MSB = true)
--                 or alternatively:
--                 in3 & in2 & in1 & in0 (for INPUT0_AT_MSN = false)
--                 If providing the input stream from a memory, it is likely
--                 that INPUT0_AT_MSN is appropriate setting.
--                 (default = false)
-- BLOCK_EXTRA_INPUT : boolean that specifies whether or not the buffer should
--                     prevent extra inputs from being stored by deasserting
--                     the ready signal after reading all inputs. This
--                     options simplifies the processing of multiple images
--                     (e.g. video) because there is an period of time between
--                     when the last pixel of an image enters the generator
--                     and when the generator is ready to accept a new image.
--                     If false, the user has to track the number of pixels in
--                     the input stream to ensure that pixels from the next
--                     image are not sent to the generator until the generator
--                     has asserted done.
--                     If true, the window generator simplifies
--                     external logic because it will deassert ready after
--                     reading the entire image, which will inform the user
--                     that the generator is not ready for the next image.
--                     We recommend setting this value to true, which simplies
--                     control at the cost of  a small amount of logic and
--                     a multiplier.
--                     (default = true)
--
-- (ACTUAL INPUT SIZES)
-- IROWS : the actual number of image rows for the simulation
-- ICOLS : the actual number of image cols for the simulation
-- WROWS : the actual number of window rows for the simulation
-- WCOLS : the actual number of window cols for the simulation
--
-- (PARAMETERS FOR READ AND WRITE TIMINGS DURIGN SIMULATION)
-- DELAY_PROP_WRITE : The probability of a write data (i.e. inputs) not being
--                    available when the generator is ready.
--                    Valid range: 0.0 to 1.0.
-- MIN_RW_DELAY : When a write is delayed, this is the minimum number of cycles
--                for the delay.
-- MAX_RW_DELAY : When a write is delayed, this is the maximum number of cycles
--                for the delay.
-- DELAY_PROP_READ : The probability of a read being delay when output window
--                   are available.
--                   Valid range: 0.0 to 1.0.
-- MIN_RD_DELAY : When a read is delayed, this is the minimum number of cycles
--                for the delay.
-- MAX_RD_DELAY : When a read is delayed, this is the maximum number of cycles
--                for the delay.
--
-- (MISC SIMULATION PARAMETERS)
-- CLK_PERIOD : The clock period used for the generator
-- NUM_IMAGES : The number of images to test
-- CHANGE_DIMENSION : If true, this will randomly select different image
--                    dimensions for each image.
--
-- (DEBUGGING AND PERFORMANCE ANALYSIS)
-- TEST_NAME : A string specifying the name of one instance of the testbench.
--             Useful when instantiating this test within a larger set of tests.
-- PRINT_STATUS : If true, enables reports during the simulation. Useful to
--                determine the current point of the simulation.
-- LOG_EN : It true, prints performance information to a log file.
-- LOG_FILE : The name of the log file. Ignored if LOG_EN = false.
-------------------------------------------------------------------------------

entity window_gen_tb is
    generic(
        -- window generator generics
        PARALLEL_IO       : positive := 3;
        MAX_WINDOW_ROWS   : positive := 5;
        MAX_WINDOW_COLS   : positive := 5;
        MAX_IMAGE_ROWS    : positive := 80;
        MAX_IMAGE_COLS    : positive := 80;
        DATA_WIDTH        : positive := 16;
        INPUT0_AT_MSB     : boolean  := true;
        BLOCK_EXTRA_INPUT : boolean  := true;

        -- Actual input sizes
        IROWS : in positive := 20;
        ICOLS : in positive := 20;
        WROWS : in positive := 5;
        WCOLS : in positive := 5;

        -- parameters for read and writing timings in testbench
        DELAY_PROB_WRITE : real    := 0.1;
        MIN_WR_DELAY     : natural := 0;
        MAX_WR_DELAY     : natural := 10;

        DELAY_PROB_READ : real    := 0.1;
        MIN_RD_DELAY    : natural := 0;
        MAX_RD_DELAY    : natural := 10;

        -- misc simulation parameters
        CLK_PERIOD       : time     := 10 ns;
        NUM_IMAGES       : positive := 5;
        CHANGE_DIMENSION : boolean  := false;

        -- debugging and performance analysis
        TEST_NAME    :    string  := "DEFAULT_TEST";
        PRINT_STATUS : in boolean := false;
        LOG_EN       : in boolean := false;
        LOG_FILE     : in string  := "sim_times.txt"
        );
end window_gen_tb;


architecture TB of window_gen_tb is

    type window_t is array(0 to MAX_WINDOW_ROWS-1, 0 to MAX_WINDOW_COLS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    type window_array_t is array (0 to PARALLEL_IO-1) of window_t;

    signal stop_clk : std_logic := '0';
    signal sim_done : std_logic := '0';

    signal clk         : std_logic := '0';
    signal rst         : std_logic := '0';
    signal go          : std_logic := '0';
    signal input_valid : std_logic := '0';
    signal ready       : std_logic;
    signal read_enable : std_logic := '0';
    signal empty       : std_logic;

    signal image_rows  : std_logic_vector(bitsNeeded(MAX_IMAGE_ROWS)-1 downto 0)  := (others => '0');
    signal image_cols  : std_logic_vector(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0)  := (others => '0');
    signal window_rows : std_logic_vector(bitsNeeded(MAX_WINDOW_ROWS)-1 downto 0) := (others => '0');
    signal window_cols : std_logic_vector(bitsNeeded(MAX_WINDOW_COLS)-1 downto 0) := (others => '0');

    signal input        : std_logic_vector(DATA_WIDTH*PARALLEL_IO-1 downto 0) := (others => '0');
    signal output       : std_logic_vector(MAX_WINDOW_ROWS*(MAX_WINDOW_COLS+PARALLEL_IO-1)*DATA_WIDTH-1 downto 0);
    signal window_valid : std_logic_vector(PARALLEL_IO-1 downto 0);
    signal done         : std_logic;

    signal in_count_s, count_s, write_count_s : std_logic_vector(DATA_WIDTH-1 downto 0);

    signal windows_per_row    : positive;
    signal total_windows      : positive;
    signal images_completed_s : natural   := 0;
    signal active             : std_logic := '0';

    signal i_rows : positive;
    signal i_cols : positive;
    signal w_rows : positive;
    signal w_cols : positive;

    -- used for viewing output in simulation
    signal window_array  : window_array_t;
    signal correct_array : window_array_t;

    signal delay_read : std_logic := '0';

    -----------------------------------------------------------------------
    -- Procedure devectorizeWindow
    -- Description: convert a 1D vectorized representation of an output window
    --              into a corresponding 2D array for easier processing. Note
    --              that this always determines a window of size
    --              MAX_WINDOW_ROWS X MAX_WINDOW_COLS. For smaller windows,
    --              just ignore the extra rows and columns.
    --
    -- Parameters:
    -- vector : The 1D-vectorized version of the 2D array, stored in row-major
    --          order. Index (0,0) starts at the MSB in the vector, with the
    --          LSB storing the end of index (total_rows-1, total_cols-1)
    -- window : the window as a 2D array (window_t)
    -- index : In case multiple windows are specified in the output, index
    --         specifies which one to get
    --
    -- Preconditions: index < PARALLEL_IO, vector must be the
    -- appropriate size with data stored as described above.
    -----------------------------------------------------------------------

    procedure devectorizeWindow(signal vector :     std_logic_vector;
                                window        : out window_t;
                                index         :     natural) is

    begin
        for i in 0 to MAX_WINDOW_ROWS-1 loop
            for j in index to MAX_WINDOW_COLS+index-1 loop
                window(i, j-index) := getVectorElement(vector, i, j, MAX_WINDOW_ROWS, MAX_WINDOW_COLS+PARALLEL_IO-1, DATA_WIDTH);
--                window(i, j) <= (others => '0');
            end loop;
        end loop;
    end devectorizeWindow;

    -----------------------------------------------------------------------
    -- Function getCorrectWindow
    -- Description: This function generates the correct window based on the
    --              specified output window count. The current implementation
    --              assumes that the input uses sequential values starting at 0.
    --
    -- Parameters:
    -- window_count : Specifies the output window (starting at 0)
    -- image_cols : the number of columns in the image being tested
    -- window_rows : the number of rows in the window being tested
    -- window_cols : the number of columns in the window being tested
    --
    -- Preconditions: image_cols >= window_cols
    -----------------------------------------------------------------------    

    function getCorrectWindow(window_count       : natural;
                              signal image_cols  : positive;
                              signal window_rows : positive;
                              signal window_cols : positive) return window_t is
        variable window          : window_t;
        variable windows_per_row : integer;
        variable start_row       : integer;
        variable start_col       : integer;
    begin
        assert(image_cols >= window_cols);

        windows_per_row := image_cols-window_cols+1;
        start_row       := window_count / windows_per_row;
        start_col       := window_count mod windows_per_row;

        for i in start_row to start_row+window_rows-1 loop
            for j in start_col to start_col+window_cols-1 loop
--                report "count=" & integer'image(window_count) & " i=" & integer'image(i) & " j=" & integer'image(j);
                window(i-start_row, j-start_col) := std_logic_vector(to_unsigned(i*image_cols + j, DATA_WIDTH));
            end loop;
        end loop;

        return window;
    end getCorrectWindow;

    -----------------------------------------------------------------------
    -- Function areWindowsEqual
    -- Description: Returns true if the specified windows are equal, false
    --              otherwise 
    --
    -- Parameters:
    -- w1 : the first window to compare 
    -- w2 : the second window to compare
    -- rows : the number of rows in the windows being compared
    -- cols : the number of columns in the windows being compared
    -----------------------------------------------------------------------

    function areWindowsEqual(w1          : window_t;
                             w2          : window_t;
                             signal rows : positive;
                             signal cols : positive) return boolean is
    begin
        for i in 0 to rows-1 loop
            for j in 0 to cols-1 loop
                if (w1(i, j) /= w2(i, j)) then
--                    report "ERROR: (" & integer'image(i) & "," & integer'image(j) & ") " & integer'image(to_integer(unsigned(w1(i,j)))) & " != " & integer'image(to_integer(unsigned(w2(i,j))));

                    return false;
                end if;
            end loop;
        end loop;

        return true;

    end areWindowsEqual;

begin

    UUT : entity work.window_gen
        generic map(
            PARALLEL_IO       => PARALLEL_IO,
            MAX_WINDOW_ROWS   => MAX_WINDOW_ROWS,
            MAX_WINDOW_COLS   => MAX_WINDOW_COLS,
            MAX_IMAGE_ROWS    => MAX_IMAGE_ROWS,
            MAX_IMAGE_COLS    => MAX_IMAGE_COLS,
            DATA_WIDTH        => DATA_WIDTH,
            INPUT0_AT_MSB     => INPUT0_AT_MSB,
            BLOCK_EXTRA_INPUT => BLOCK_EXTRA_INPUT
            )
        port map(
            clk          => clk,
            rst          => rst,
            go           => go,
            ready        => ready,
            read_enable  => read_enable,
            empty        => empty,
            image_rows   => image_rows,
            image_cols   => image_cols,
            window_rows  => window_rows,
            window_cols  => window_cols,
            input        => input,
            input_valid  => input_valid,
            output       => output,
            window_valid => window_valid,
            done         => done
            );

    -- generate the clock until the simulation finishes
    clk <= not clk after CLK_PERIOD/2 when (stop_clk = '0') else '0';

    -- Send inputs to window buffer. All inputs are consecutive starting at 0
    -- to make checking for correct outputs easier
    process
        variable in_count, write_count : integer;
        variable packed_input          : std_logic_vector(PARALLEL_IO*DATA_WIDTH-1 downto 0);
        variable s1, s2, s3, s4        : positive;  -- seed values
        variable im_rows, im_cols      : integer;

        variable start_time, sim_time : time := 0 ns;
        variable file_line            : line;
        variable line_var             : line;
        file text_file                : text;
    begin
        if (LOG_EN) then
            file_open(text_file, LOG_FILE, append_mode);
        end if;

        --Reset the entity
        rst    <= '1';
        go     <= '0';
        active <= '0';

        -- Loop through the process until NUM_IMAGES has been completed,
        -- waiting for 'done' at the end of each iteration        
        while(images_completed_s < NUM_IMAGES) loop

            i_rows <= IROWS;
            i_cols <= ICOLS;
            w_rows <= WROWS;
            w_cols <= WCOLS;

            -- Randomly change image dimensions if CHANGE_DIMENSION = true
            if(images_completed_s > 0 and CHANGE_DIMENSION = true) then
                randomInt(s3, s4, MAX_WINDOW_ROWS, MAX_IMAGE_ROWS, im_rows);
                randomInt(s3, s4, MAX_WINDOW_COLS, MAX_IMAGE_COLS, im_cols);
                i_rows <= im_rows;
                i_cols <= im_cols;
            end if;

            -- determine when the generator should be done
            wait until rising_edge(clk);
            windows_per_row <= i_cols-w_cols+1;

            wait until rising_edge(clk);
            total_windows <= windows_per_row*(i_rows-w_rows+1);

            -- specify the genertor input dimensiosn
            image_rows  <= std_logic_vector(to_unsigned(i_rows, image_rows'length));
            image_cols  <= std_logic_vector(to_unsigned(i_cols, image_cols'length));
            window_rows <= std_logic_vector(to_unsigned(w_rows, window_rows'length));
            window_cols <= std_logic_vector(to_unsigned(w_cols, window_cols'length));

            for i in 0 to 5 loop
                wait until rising_edge(clk);
            end loop;

            if (PRINT_STATUS) then
                report "IMAGE IS PROCESSING: " & TEST_NAME & ": rows = " & integer'image(i_rows) & ", cols = " & integer'image(i_cols) & ".";
            end if;

            rst <= '0';
            go  <= '0';
            wait until rising_edge(clk);

            -- start the buffer with the specified inputs
            go         <= '1';
            active     <= '1';
            start_time := now;
            wait until rising_edge(clk);

            in_count    := 0;
            write_count := 0;
            while (in_count < i_rows*i_cols) loop

                -- Randomize delay before sending the next input
                input_valid <= '0';
                randDelay(s1, s2, clk, DELAY_PROB_WRITE, MIN_WR_DELAY, MAX_WR_DELAY);

                -- pack PARALLEL_IO inputs into a group based on the specified
                -- input ordering
                if (INPUT0_AT_MSB) then
                    for i in PARALLEL_IO-1 downto 0 loop
                        packed_input((i+1)*DATA_WIDTH-1 downto i*DATA_WIDTH) := std_logic_vector(to_unsigned(in_count, DATA_WIDTH));

                        in_count   := in_count + 1;
                        in_count_s <= std_logic_vector(to_unsigned(in_count, in_count_s'length));
                    end loop;
                else
                    for i in 0 to PARALLEL_IO-1 loop
                        packed_input((i+1)*DATA_WIDTH-1 downto i*DATA_WIDTH) := std_logic_vector(to_unsigned(in_count, DATA_WIDTH));

                        in_count   := in_count + 1;
                        in_count_s <= std_logic_vector(to_unsigned(in_count, in_count_s'length));
                    end loop;
                end if;

                -- send input to generator
                input         <= packed_input;
                input_valid   <= '1';
                write_count   := write_count + 1;
                write_count_s <= std_logic_vector(to_unsigned(write_count, write_count_s'length));

                -- wait until the generator reads the input
                wait until ready = '1' and rising_edge(clk);

                -- Change the dimensions to make sure this doesn't affect
                -- correctness during exection, the generator should only see
                -- changes when go is first asserted.
                randomInt(s3, s4, MAX_WINDOW_ROWS, MAX_IMAGE_ROWS, im_rows);
                randomInt(s3, s4, MAX_WINDOW_COLS, MAX_IMAGE_COLS, im_cols);
                image_rows <= std_logic_vector(to_unsigned(im_rows, image_rows'length));
                image_cols <= std_logic_vector(to_unsigned(im_cols, image_cols'length));

            end loop;

            if (done = '0') then wait until done = '1'; end if;
            active <= '0';

            -- Allow time for the images_completed signal to update so that another iteration isnt wrongly performed
            for i in 0 to 10 loop
                wait until rising_edge(clk);
            end loop;

        end loop;

        -- wait for entire simulation to complete
        if (sim_done = '0') then wait until sim_done = '1'; end if;

        -- write simulation info to log               
        sim_time := now - start_time;
        if (LOG_EN) then
            write(line_var, TEST_NAME);
            write(line_var, string'(","));
            write(line_var, integer'image(PARALLEL_IO));
            write(line_var, string'(","));
            write(line_var, integer'image(MAX_WINDOW_ROWS));
            write(line_var, string'(","));
            write(line_var, integer'image(MAX_WINDOW_COLS));
            write(line_var, string'(","));
            write(line_var, integer'image(MAX_IMAGE_ROWS));
            write(line_var, string'(","));
            write(line_var, integer'image(MAX_IMAGE_COLS));
            write(line_var, string'(","));
            write(line_var, DATA_WIDTH);
            write(line_var, string'(","));
            write(line_var, integer'image(i_rows));
            write(line_var, string'(","));
            write(line_var, integer'image(i_cols));
            write(line_var, string'(","));
            write(line_var, integer'image(w_rows));
            write(line_var, string'(","));
            write(line_var, integer'image(w_cols));
            write(line_var, string'(","));
            write(line_var, DELAY_PROB_WRITE);
            write(line_var, string'(","));
            write(line_var, DELAY_PROB_READ);
            write(line_var, string'(","));
            write(line_var, sim_time);
            writeline(text_file, line_var);

            file_close(text_file);
        end if;

        stop_clk <= '1';
        report "SIMULATION DONE!!!!";

        wait;
    end process;

    -- Randomize a read delay to test buffer filling up and having to stall
    process
        variable s1, s2       : positive;
        variable should_delay : boolean;
    begin
        delay_read <= '1';
        randDelay(s1, s2, clk, DELAY_PROB_READ, MIN_RD_DELAY, MAX_RD_DELAY);
        delay_read <= '0';
        wait until rising_edge(clk);
    end process;

    -- reads any time the buffer isn't empty and not being delayed. 
    read_enable <= not empty and not delay_read and active;

    -- check if the outputs are correct
    process
        variable actual_window    : window_t;
        variable correct_window   : window_t;
        variable output_count     : natural := 0;
        variable s1, s2           : positive;
        variable images_completed : natural := 0;

    begin

        -- if read_enable is asserted on a rising clock edge the current
        -- window that is being output from the generator should be correct.
        -- Note that the output changes immediately after the rising clock
        -- edge, so we can't have any wait statements before the check.
        wait until rising_edge(clk);
        if (read_enable = '1') then

            -- check all PARALLEL_IO windows
            for i in 0 to PARALLEL_IO-1 loop

                -- if the window is valid, check it, otherwise ignore it
                if (window_valid(PARALLEL_IO-i-1) = '1') then

                    -- get 2D array version of window
                    devectorizeWindow(output, actual_window, i);
                    -- enable output to be seen in simulation
                    window_array(i) <= actual_window;

                    -- get correct window based on output_count
                    correct_window   := getCorrectWindow(output_count, i_cols, w_rows, w_cols);
                    -- enable correct window to be seen in simulation
                    correct_array(i) <= correct_window;

                    -- check if the window is correct
                    if (areWindowsEqual(actual_window, correct_window, w_rows, w_cols) = false) then
                        report "ERROR: Window " & integer'image(i) & " is incorrect." severity failure;
                    end if;

                    output_count := output_count + 1;
                    count_s      <= std_logic_vector(to_unsigned(output_count, count_s'length));
                end if;

            end loop;
        end if;

        -- check for correct done status
        if (output_count = total_windows) then

            wait until rising_edge(clk);
            images_completed := images_completed+1;
            assert(done = '1') report "ERROR: Done not asserted upon completion - " & TEST_NAME & ": rows = " & integer'image(i_rows) & ", cols = " & integer'image(i_cols) & "." severity failure;

            output_count := 0;
            if (PRINT_STATUS) then
                report "IMAGE PROCESSED SUCCESSFULLY: " & TEST_NAME & ": rows = " & integer'image(i_rows) & ", cols = " & integer'image(i_cols) & ".";
            end if;

            if(images_completed = NUM_IMAGES) then
                sim_done <= '1';
            end if;

        else
            if (active = '1') then
                assert(done = '0') report "ERROR: Done asserted before completion - " & TEST_NAME & ": " & integer'image(i_rows) & ", cols = " & integer'image(i_cols) & "." severity failure;
            end if;
        end if;

        images_completed_s <= images_completed;

    end process;

    -- check to make sure that lower window_valid bits are never asserted when
    -- higher bits aren't asserted.
    process
    begin
        while(sim_done = '0') loop
            for i in 0 to PARALLEL_IO-2 loop
                if(window_valid(i) = '1' and window_valid(i+1) = '0') then
                    report "ERROR - lower_window valid bits are '1' when upper bits are '0'!" severity failure;
                end if;
                wait until rising_edge(clk);

            end loop;
        end loop;

        wait;
    end process;

end TB;



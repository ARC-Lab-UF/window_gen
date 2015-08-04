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

-- Greg Stitt
-- University of Florida

-- Description: This entity generates configurable-sized windows from a
-- sequential stream of data. The generator provides a FIFO-like interface
-- with empty and read (ren) signals. Instead of providing a write enable,
-- the generator uses an input_valid signal that allows the user to specify when
-- inputs are valid. The generator provides a ready output that informs the
-- user when inputs are accepted. On any rising clock edge where the user
-- asserts input_valid while the generator asserts ready, the generator will
-- store that input.
--
-- The generator supports dynamically configurable image sizes and window sizes.
-- Static configuration options include a maximum image and window size, in
-- addition to the number of parallel inputs and outputs, which also
-- specifies the maximum number of windows produced each cycle. Note that
-- the generator requires an additional parallel input for each parallel window
-- output. The generator could potentially be extended to allow for more inputs
-- than outputs, but this extension would cause the internal buffering to fill
-- up faster than the outputs could empty it. If the user can provide more
-- inputs than the number of desired parallel windows, it would be best to use
-- a FIFO on the input stream to stall the stream before entering the generator.
--
-- The generator currently outputs windows that are "fully immersed" within
-- the image. Some applications (e.g., convolution) slide windows outside the
-- boundaries of the image. To achieve this functionality, pad the original
-- image and increase the specified image size accordingly. The generator will
-- then create the corresponding windows that exceed the boundaries of the
-- original image with the desired padding values. 

-- Limitations: requires the specified image to be larger than the maximum
-- window size. This limitation should only affect applications with very small
-- images or very large windows. If this limitation is a problem, pad the image
-- to match the maximum window size.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generics Description
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
--                 that false is an appropriate setting.
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
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk: Clock
-- rst: Asynchronous reset
-- go : Starts the generation of windows using the specified image and window
--      sizes
-- ready : Asserted by the generator to specify that it is ready to accept
--         input. If the user asserts input_valid on a rising edge where ready
--         is also asserted, then the generator will store that input and the
--         user can move to the next input. Any inputs provided when ready is
--         not asserted will not be stored into the generator. Therefore, the
--         user should not move to the next output when ready = '0'.
-- read_enable : The user should assert when reading a window(s) from the
--               generator. There is no delay for a read. The window is
--               already on the output port. This signal simply tells the
--               buffer to generate the next window. In the case of
--               PARALLEL_IO > 1, asserting read_enable reads PARALLEL_IO
--               windows.
-- empty : '1' when the buffer has at least one valid window, '0' otherwise
-- image_rows : the number of rows in the input image. Must be <=
--              MAX_IMAGE_ROWS and >= MAX_WINDOW_ROWS
-- image_cols : the number of cols in the input image. Must be <=
--              MAX_IMAGE_COLS and >= MAX_WINDOW_COLS
-- window_rows : the number of rows in the generated window. Must be <=
--               MAX_WINDOW_ROWS
-- window_cols : the number of rows in the generated window. Must be <=
--               MAX_WINDOW_COLS
-- input : The current DATA_WIDTH-bit element of the input stream, or
--         PARALLEL_IO inputs in the case of PARALLEL_IO > 1. Note that the
--         order of parallel inputs can be configured with the INPUT0_AT_MSB
--         option.
-- input_valid : should be asserted by the user when "input" is valid. In the
--               case of PARALLEL_IO > 1, this specifies that PARALLEL_IO
--               inputs are valid.
-- output : A generated window, or PARALLEL_IO windows when PARALLEL_IO > 1.
--          The output always produces a maximum-sized window. When smaller
--          windows are requested, ignore the elements outside the requested
--          window. In the case of PARALLEL_IO > 1, the number of rows in the
--          output is MAX_WINDOW_ROWS+PARALLEL_IO-1 to account for the multiple
--          windows. The first window starts in column 0, the second in column
--          1, etc. All windows start in row 0.
--
--          Note that the output is "vectorized" into a 1D vector to avoid
--          limitations of pre-2008 VHDL. The top-left element of the first
--          window (0,0) is stored in the highest bits of the vector. The rest
--          of the elements appear in row-major order.
--
--          We have provided a basic template (template.vhd) to demonstrate
--          how to more conveniently access the window outputs.
-- window_valid : Specifies which of the PARALLEL_IO windows on the output
--                signal are valid. When PARALLEL_IO > 1, there will be
--                situations where some windows are invalid due to various
--                situations (e.g., windows exceeding the current row of the
--                image). The MSB corresponds to the earliest window (the one
--                starting in column 0 of the output), and the LSB corresponds
--                to the latest window (the one starting in column
--                PARALLEL_IO-1 of the output)
-- done : Asserted by the generator when all windows have been generated
--        (but not read from the buffer). In other words, if there are less than
--        PARALLEL_IO windows left to generate, and there are the same number
--        of valid windows left in the buffer, done will be asserted.
-------------------------------------------------------------------------------

entity window_gen is
    generic (
        PARALLEL_IO       : positive;
        MAX_WINDOW_ROWS   : positive;
        MAX_WINDOW_COLS   : positive;
        MAX_IMAGE_ROWS    : positive;
        MAX_IMAGE_COLS    : positive;
        DATA_WIDTH        : positive;
        INPUT0_AT_MSB     : boolean := false;
        BLOCK_EXTRA_INPUT : boolean := true);
    port (
        clk          : in  std_logic;
        rst          : in  std_logic;
        go           : in  std_logic;
        ready        : out std_logic;
        read_enable  : in  std_logic;
        empty        : out std_logic;
        image_rows   : in  std_logic_vector(bitsNeeded(MAX_IMAGE_ROWS)-1 downto 0);
        image_cols   : in  std_logic_vector(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0);
        window_rows  : in  std_logic_vector(bitsNeeded(MAX_WINDOW_ROWS)-1 downto 0);
        window_cols  : in  std_logic_vector(bitsNeeded(MAX_WINDOW_COLS)-1 downto 0);
        input        : in  std_logic_vector(DATA_WIDTH*PARALLEL_IO-1 downto 0);
        input_valid  : in  std_logic;
        output       : out std_logic_vector(MAX_WINDOW_ROWS*(MAX_WINDOW_COLS+PARALLEL_IO-1)*DATA_WIDTH-1 downto 0);
        window_valid : out std_logic_vector(PARALLEL_IO-1 downto 0);
        done         : out std_logic);
end window_gen;

architecture STR of window_gen is

    signal window_buffer_rst     : std_logic;
    signal window_buffer_empty   : std_logic;
    signal window_buffer_is_edge : std_logic_vector(PARALLEL_IO-1 downto 0);
    signal window_buffer_output  : std_logic_vector(DATA_WIDTH*PARALLEL_IO*MAX_WINDOW_ROWS-1 downto 0);
    signal window_buffer_done    : std_logic;

    signal coalescer_rst          : std_logic;
    signal coalescer_read_request : std_logic;
    signal coalescer_input_ready  : std_logic;

    signal fifo_vr_rst       : std_logic;
    signal fifo_vr_rd        : std_logic;
    signal fifo_vr_rd_amount : std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
--    signal fifo_vr_empty     : std_logic;
    signal fifo_vr_full      : std_logic;
    signal fifo_vr_output    : std_logic_vector(parallel_io*data_width-1 downto 0);
    signal fifo_vr_count     : std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);

    signal cols_remaining, col : integer range 0 to MAX_IMAGE_COLS;
    signal rd_amount           : integer range 0 to PARALLEL_IO;

    signal ready_s                   : std_logic;
    signal read_input_s              : std_logic;
    signal window_buffer_input_ready : std_logic;
    signal valid_rd                  : std_logic;
    signal coalescer_empty           : std_logic;
    signal coalescer_done            : std_logic;

    signal r_done, done_s : std_logic;

    type state_type is (S_WAIT_FOR_GO, S_COMPUTE_BUFFER_SIZE, S_WAIT_FOR_DONE, S_WAIT_GO_CLEAR);
    constant MAX_BUFFER_SIZE : positive := positive(ceil(real(MAX_IMAGE_COLS)/real(PARALLEL_IO)));

    signal r_div_count, div_count_s     : unsigned(image_cols'length-1 downto 0);
    signal r_buffer_size, buffer_size_s : unsigned(bitsNeeded(MAX_BUFFER_SIZE)-1 downto 0);

    signal r_image_rows, image_rows_s   : std_logic_vector(bitsNeeded(MAX_IMAGE_ROWS)-1 downto 0);
    signal r_image_cols, image_cols_s   : std_logic_vector(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0);
    signal r_window_rows, window_rows_s : std_logic_vector(bitsNeeded(MAX_WINDOW_ROWS)-1 downto 0);
    signal r_window_cols, window_cols_s : std_logic_vector(bitsNeeded(MAX_WINDOW_COLS)-1 downto 0);

    signal r_buffer_size_ready, buffer_size_ready_s : std_logic;
    signal r_active, active_s                       : std_logic;
    signal state, next_state                        : state_type;

    signal block_input : std_logic;

begin

    ---------------------------------------------------------------------
    -- Control
    -- State machine that waits for go, saves the image and row
    -- sizes in regsiters, computes the appropriate buffer size based on
    -- the image size, and then waits for all windows to be generated
    -- before asserting done
    --------------------------------------------------------------------

    process(clk, rst)
    begin
        if (rst = '1') then

            r_image_rows  <= (others => '0');
            r_image_cols  <= (others => '0');
            r_window_rows <= (others => '0');
            r_window_cols <= (others => '0');

            r_div_count         <= (others => '0');
            r_buffer_size       <= (others => '0');
            r_buffer_size_ready <= '0';

            r_active <= '0';
            r_done   <= '0';

            state <= S_WAIT_FOR_GO;

        elsif (rising_edge(clk)) then

            r_image_rows  <= image_rows_s;
            r_image_cols  <= image_cols_s;
            r_window_rows <= window_rows_s;
            r_window_cols <= window_cols_s;

            r_div_count         <= div_count_s;
            r_buffer_size       <= buffer_size_s;
            r_buffer_size_ready <= buffer_size_ready_s;

            r_active <= active_s;
            r_done   <= done_s;

            state <= next_state;
        end if;
    end process;

    process(rst, state, go, r_div_count, r_buffer_size, r_image_rows, r_image_cols, r_window_rows, r_window_cols, r_done, r_buffer_size_ready, r_active, image_rows, image_cols, window_rows, window_cols, coalescer_done)
    begin

        fifo_vr_rst       <= rst;
        window_buffer_rst <= rst;
        coalescer_rst     <= rst;

        image_rows_s  <= r_image_rows;
        image_cols_s  <= r_image_cols;
        window_rows_s <= r_window_rows;
        window_cols_s <= r_window_cols;

        div_count_s         <= r_div_count;
        buffer_size_s       <= r_buffer_size;
        buffer_size_ready_s <= r_buffer_size_ready;

        active_s <= r_active;
        done_s   <= r_done;

        next_state <= state;

        case state is
            when S_WAIT_FOR_GO =>

                active_s      <= '0';
                buffer_size_s <= to_unsigned(0, buffer_size_s'length);

                if (go = '1') then

                    -- register inputs to avoid problems if user changes
                    -- them during execution
                    image_rows_s  <= image_rows;
                    image_cols_s  <= image_cols;
                    window_rows_s <= window_rows;
                    window_cols_s <= window_cols;

                    -- reset all helper entities
                    fifo_vr_rst       <= '1';
                    window_buffer_rst <= '1';
                    coalescer_rst     <= '1';

                    -- immediately change the done signal on go
                    done_s <= '0';

                    -- initialize divider computation
                    div_count_s         <= unsigned(image_cols);
                    buffer_size_ready_s <= '0';

                    next_state <= S_COMPUTE_BUFFER_SIZE;
                end if;

            -- calculate the size of each row buffer
            -- r_buffer_size = ceil(image_cols % PARALLEL_IO);
            -- This state uses subtraction to avoid a divider. Although this
            -- code delays the start of the buffer, it only has to be done
            -- once for a complete slide of the window across the image.
            -- TODO: this isn't needed when PARALLEL_IO is a power of two,
            -- which should be added as an if generate
            when S_COMPUTE_BUFFER_SIZE =>

                active_s <= '1';

                if (r_div_count >= PARALLEL_IO) then
                    div_count_s   <= r_div_count - PARALLEL_IO;
                    buffer_size_s <= r_buffer_size + 1;
                elsif (r_div_count > 0) then
                    buffer_size_s <= r_buffer_size + 1;
                    next_state    <= S_WAIT_FOR_DONE;
                else
                    next_state <= S_WAIT_FOR_DONE;
                end if;

            when S_WAIT_FOR_DONE =>

                -- flag specifying completion of the buffer size computation
                buffer_size_ready_s <= '1';

                -- waiting until all windows have been generated
                if (coalescer_done = '1') then
                    done_s     <= '1';
                    active_s   <= '0';
                    next_state <= S_WAIT_GO_CLEAR;
                end if;

            when S_WAIT_GO_CLEAR =>
                -- ensures that the generator won't start again if go is
                -- continually asserted. It will only restart after go is
                -- deasserted for at least one cycle and then reasserted.
                if (go = '0') then
                    next_state <= S_WAIT_FOR_GO;
                end if;
        end case;
    end process;

    -- the generator is ready when the vr fifo isn't full, the buffer size has
    -- been computed, and the block_input flag isn't asserted.
    ready_s <= not fifo_vr_full and r_buffer_size_ready and not block_input;
    ready   <= ready_s;

    -- the vr fifo should read the input  anytime there is a valid input and
    -- the generator is ready
    read_input_s <= input_valid and ready_s;

    --------------------------------------------------------------------
    -- FIFO_VR
    -- writes PARALLEL_IO inputs at a time and reads a variable amount up to
    -- PARALLEL_IO depending on location of the current inputs in the image.
    -- e.g. If reading PARALLEL_IO inputs exceeds the current row of the image,
    -- this entity enables fewer elements to be read.
    --------------------------------------------------------------------

    U_FIFO_VR : entity work.fifo_vr
        generic map (
            data_width     => DATA_WIDTH,
            parallel_io    => PARALLEL_IO,
            input0_at_MSB  => INPUT0_AT_MSB,
            output0_at_MSB => true)
        port map (
            clk       => clk,
            rst       => fifo_vr_rst,
            rd        => fifo_vr_rd,
            rd_amount => fifo_vr_rd_amount,
            wr        => read_input_s,
--            empty     => fifo_vr_empty,
            empty     => open,
            full      => fifo_vr_full,
            input     => input,
            output    => fifo_vr_output,
            count     => fifo_vr_count,
--            count     => open,
            valid_out => open
            );

    U_BLOCK_INPUT : if BLOCK_EXTRA_INPUT = true generate
        -- track writes into fifo_vr
        -- This is needed to know when to block the input
        process(clk, fifo_vr_rst)
            -- slight hack: use variables to avoid unused signal warnings when
            -- generics disable this process
            constant MAX_IN_COUNT : positive := positive(ceil(real(MAX_IMAGE_COLS*MAX_IMAGE_ROWS)/real(PARALLEL_IO))) * PARALLEL_IO;
            variable in_count     : integer range 0 to MAX_IN_COUNT;

        begin
            if (fifo_vr_rst = '1') then
                in_count    := 0;
                block_input <= '0';

            elsif (rising_edge(clk)) then
                if (read_input_s = '1') then

                    -- NOTE: can cause harmless latch warning when
                    -- PARALLEL_IO is even because in_count(0) will always
                    -- be 0.
                    in_count := in_count + PARALLEL_IO;
                    if (in_count >= unsigned(r_image_cols)*unsigned(r_image_rows)) then
                        block_input <= '1';
                    end if;
                end if;
            end if;
        end process;
    end generate;

    U_NO_BLOCK_INPUT : if BLOCK_EXTRA_INPUT = false generate
        block_input <= '0';
    end generate;

    -- track the current column index for each read from fifo_vr
    -- This is needed to know how many columns remain in the current row
    process(clk, rst)
    begin
        if (rst = '1') then
            col <= 0;
        elsif (rising_edge(clk)) then
            if (fifo_vr_rd = '1') then
                if (col + rd_amount < unsigned(r_image_cols)) then
                    col <= col + rd_amount;
                else
                    -- reset the column for the next row
                    col <= 0;
                end if;
            end if;
        end if;
    end process;

    -- determine columns remaining in current row
    cols_remaining <= to_integer(unsigned(r_image_cols)) - col;

    -- always read PARALLEL_IO inputs, unless that exceeds the end of a row
    rd_amount <= PARALLEL_IO when cols_remaining >= PARALLEL_IO else cols_remaining;

--    assert(rd_amount <= unsigned(fifo_vr_count)) report "ERROR: FIFO_VR reading more data than is available.";

    -- convert rd_amount to slv for fifo_vr input
    fifo_vr_rd_amount <= std_logic_vector(to_unsigned(rd_amount, fifo_vr_rd_amount'length));

    -- the input to the window_buffer is ready anytime the variable FIFO has
    -- enough data to read. It is important not to just check for empty because
    -- the fifo_vr might have data, but not as much as the window_buffer expects
--    window_buffer_input_ready <= not fifo_vr_empty and (unsigned(fifo_vr_count) >= rd_amount);
    window_buffer_input_ready <= '1' when unsigned(fifo_vr_count) >= rd_amount else '0';

    --------------------------------------------------------------------
    -- WG_BUFFER (Window Buffer)
    -- array of fifos, one for each row of the window where each fifo is the
    -- size of a row (the number of columns) in the image when PARALLEL_IO = 1.
    -- When PARALLEL_IO > 1, each FIFO has ceil(image_cols%PARALLEL_IO)
    -- elements because each FIFO entry stores PARALLEL_IO inputs.
    --------------------------------------------------------------------

    U_BUFFER : entity work.wg_buffer
        generic map (
            PARALLEL_IO     => PARALLEL_IO,
            DATA_WIDTH      => DATA_WIDTH,
            MAX_WINDOW_ROWS => MAX_WINDOW_ROWS,
            MAX_WINDOW_COLS => MAX_WINDOW_COLS,
            MAX_IMAGE_ROWS  => MAX_IMAGE_ROWS,
            MAX_IMAGE_COLS  => MAX_IMAGE_COLS,
            MAX_BUFFER_SIZE => MAX_BUFFER_SIZE)
        port map (
            clk         => clk,
            rst         => window_buffer_rst,
            active      => r_active,
            ren         => coalescer_read_request,
            input_ready => window_buffer_input_ready,
            read_input  => fifo_vr_rd,
            empty       => window_buffer_empty,
            is_edge     => window_buffer_is_edge,
            buffer_size => std_logic_vector(r_buffer_size),
            image_rows  => r_image_rows,
            image_cols  => r_image_cols,
            window_rows => r_window_rows,
            window_cols => r_window_cols,
            input       => fifo_vr_output,
            output      => window_buffer_output,
            done        => window_buffer_done);


    --------------------------------------------------------------------
    -- WB_COALESCER (Window Coalescer)
    -- 2D register buffer that coalesces columns from the window buffer
    -- into complete windows
    --------------------------------------------------------------------

    -- the coalescer input is ready any time the window_buffer is not empty,
    -- which corresponds to there being at least one element in each FIFO (i.e.
    -- one full column of the next window)
    coalescer_input_ready <= not window_buffer_empty;

    U_COALESCE : entity work.wg_coalescer
        generic map (
            PARALLEL_IO => PARALLEL_IO,
            NUM_ROWS    => MAX_WINDOW_ROWS,
            NUM_COLS    => MAX_WINDOW_COLS,
            DATA_WIDTH  => DATA_WIDTH)
        port map (
            clk          => clk,
            rst          => coalescer_rst,
            ren          => valid_rd,
            read_input   => coalescer_read_request,
            empty        => coalescer_empty,
            input        => window_buffer_output,
            input_ready  => coalescer_input_ready,
            input_done   => window_buffer_done,
            is_edge      => window_buffer_is_edge,
            output       => output,
            window_valid => window_valid,
            done         => coalescer_done);

    -- a user read is valid when the coalescer isn't empty;
    valid_rd <= read_enable and not coalescer_empty;

    -- the window buffer is empty when the register buffer is empty
    empty <= coalescer_empty;

    done <= done_s;

end STR;


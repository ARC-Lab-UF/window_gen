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
-- Patrick Cooke
-- University of Florida

-- Description: This entity defines an array of FIFOs that are used to buffer
-- rows of an image. The buffer uses MAX_WINDOW_ROWS RAMs,
-- where each RAM has MAX_IMAGE_COLS words that are DATA_WIDTH bits wide. The
-- array maintaints a front position across all FIFOs, which represent the next
-- column of elements needed by the window buffer.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.math_custom.all;
use work.window_gen_pkg.all;

-------------------------------------------------------------------------------
-- Generics Description
-- PARALLEL_IO :  Specifies the number of inputs provided at a time,
--                which also defines the number of columns output
--                from the entity.
-- DATA_WIDTH : The width in bits of each element.
-- MAX_WINDOW_ROWS : The maximum number of rows in a generated window
-- MAX_WINDOW_COLS : The maximum number of cols in a generated window
-- MAX_IMAGE_ROWS : The maximum number of rows in an input image/stream
-- MAX_IMAGE_COLS : The maximum number of cols in an input image/stream
-- MAX_BUFFER_SIZE : The maximum buffer size (should be computed by the window
--                   generator)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk : clock
-- rst : asynchronous reset
-- active : '1' if the buffer is still in the process of generating windows
-- ren : '1' to pop front values from each fifo
-- input_ready : specifies when "input" is valid
-- read_input : informs the source of the stream that the buffer is accepting data
-- empty : '1' when the buffer has a valid window, '0' otherwise
-- is_edge : '1' if the current input is the edge (left column) of a window
-- buffer_size : actual size/words of the buffer, varies based on image_cols and
--               PARALLEL_IO 
-- image_rows : the number of cols in the input image
-- image_cols : the number of cols in the input image
-- window_rows : the number of rows in the window
-- window_cols : the number of cols in the window
-- input : the next PARALLEL_IO inputs
-- output : the next PARALLEL_IO columns needed to create window
-- done : '1' when the entire image has been processed and all columns needed
--            by windows have been output
-------------------------------------------------------------------------------

entity wg_buffer is
    generic (
        PARALLEL_IO     : positive;
        DATA_WIDTH      : positive;
        MAX_WINDOW_ROWS : positive;
        MAX_WINDOW_COLS : positive;
        MAX_IMAGE_ROWS  : positive;
        MAX_IMAGE_COLS  : positive;
        MAX_BUFFER_SIZE : positive);
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;
        active      : in  std_logic;
        ren         : in  std_logic;
        input_ready : in  std_logic;
        read_input  : out std_logic;
        empty       : out std_logic;
        is_edge     : out std_logic_vector(PARALLEL_IO-1 downto 0);
        buffer_size : in  std_logic_vector(bitsNeeded(MAX_BUFFER_SIZE)-1 downto 0);
        image_rows  : in  std_logic_vector(bitsNeeded(MAX_IMAGE_ROWS)-1 downto 0);
        image_cols  : in  std_logic_vector(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0);
        window_rows : in  std_logic_vector(bitsNeeded(MAX_WINDOW_ROWS)-1 downto 0);
        window_cols : in  std_logic_vector(bitsNeeded(MAX_WINDOW_COLS)-1 downto 0);
        input       : in  std_logic_vector(DATA_WIDTH*PARALLEL_IO-1 downto 0);
        output      : out std_logic_vector(MAX_WINDOW_ROWS*PARALLEL_IO*DATA_WIDTH-1 downto 0);
        done        : out std_logic);
end wg_buffer;


architecture RTL of wg_buffer is

    type fifo_output is array (0 to MAX_WINDOW_ROWS-1) of std_logic_vector(PARALLEL_IO*DATA_WIDTH-1 downto 0);

    type fifo_array is array (0 to MAX_WINDOW_ROWS-1, 0 to PARALLEL_IO-1) of std_logic_vector(DATA_WIDTH-1 downto 0);

    -- converts from fifo_array to std_logic_vector
    function vectorizeOutput(output_array : fifo_array) return std_logic_vector is
        variable temp : std_logic_vector(MAX_WINDOW_ROWS*PARALLEL_IO*DATA_WIDTH-1 downto 0);
    begin
        for i in 0 to MAX_WINDOW_ROWS-1 loop
            for j in 0 to PARALLEL_IO-1 loop
                setVectorElement(output_array(i, j), temp, i, j, MAX_WINDOW_ROWS, PARALLEL_IO, DATA_WIDTH);
            end loop;
        end loop;

        return temp;
    end function;

    constant MAX_WINDOW_ROWS_BITS : positive := bitsNeeded(MAX_WINDOW_ROWS-1);

    -- the FIFO needs an address space going from 0 to MAX_BUFFER_SIZE-1
    constant FIFO_ADDR_BITS : positive := bitsNeeded(MAX_BUFFER_SIZE-1);

    -- pointers to the front and back FIFOs
    -- Writes always go the the back FIFO. The front represents the first row
    -- of the current windows
    signal front : unsigned(MAX_WINDOW_ROWS_BITS-1 downto 0);
    signal back  : unsigned(MAX_WINDOW_ROWS_BITS-1 downto 0);

    signal fifo_rdata       : fifo_output;
    signal fifo_rdata_array : fifo_array;
    signal fifo_wen         : std_logic_vector(MAX_WINDOW_ROWS-1 downto 0);
    signal fifo_ren         : std_logic;
    signal fifo_pop         : std_logic_vector(MAX_WINDOW_ROWS-1 downto 0);
--    signal fifo_empty      : std_logic_vector(MAX_WINDOW_ROWS-1 downto 0);
    signal fifo_read_empty  : std_logic_vector(MAX_WINDOW_ROWS-1 downto 0);
    signal fifo_read_reset  : std_logic_vector(MAX_WINDOW_ROWS-1 downto 0);
    signal fifo_full        : std_logic_vector(MAX_WINDOW_ROWS-1 downto 0);

    -- specifies when an request read is valid
    signal valid_read : std_logic;

    -- asserted when the array needs the next input
    signal get_next_input : std_logic;

    -- asserted when the front pointer changes, which corresponds to a new
    -- top row for generated windows
    signal front_changed : std_logic;

    -- the output array from all the FIFOs, which corresponds to an element
    -- from MAX_WINDOW_ROWS different rows.
    signal output_array : fifo_array;

    -- keep track of the current row and col of the input image
    signal in_row : integer range 0 to MAX_IMAGE_ROWS + MAX_WINDOW_ROWS;
    signal in_col : integer range 0 to MAX_BUFFER_SIZE*PARALLEL_IO;

    -- keep track of the current row and col of the image being used to output
    -- the current window
    signal out_row : integer range 0 to MAX_IMAGE_ROWS + MAX_WINDOW_ROWS;
    signal out_col : integer range 0 to MAX_BUFFER_SIZE*PARALLEL_IO;

    -- misc control signals (see description below
    signal input_done   : std_logic;
    signal output_done  : std_logic;
    signal stall_pop    : std_logic;
    signal almost_done  : std_logic;
    signal continue     : std_logic;
    signal image_loaded : std_logic;

    signal windows_per_row : unsigned(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0);

    signal done_s    : std_logic;
    signal is_edge_s : std_logic_vector(PARALLEL_IO-1 downto 0);
    
begin

    -- the array of fifos that acts as a circular buffer
    U_FIFOS : for i in 0 to MAX_WINDOW_ROWS-1 generate
        U_FIFO : entity work.wg_fifo
            generic map (
                DATA_WIDTH => DATA_WIDTH*PARALLEL_IO,
                ADDR_WIDTH => FIFO_ADDR_BITS)
            port map (
                clk        => clk,
                rst        => rst,
                wen        => fifo_wen(i),
                wdata      => input,
                ren        => fifo_ren,
                pop        => fifo_pop(i),
                read_reset => fifo_read_reset(i),
                read_empty => fifo_read_empty(i),
                rdata      => fifo_rdata(i),
                empty      => open,
                full       => fifo_full(i),
                size       => buffer_size);
    end generate U_FIFOS;

    -- put the PARALLEL_IO outputs stored in each FIFO word into separate array
    -- elements
    process(fifo_rdata)
    begin
        for i in 0 to MAX_WINDOW_ROWS-1 loop
            for j in 0 to PARALLEL_IO-1 loop
                fifo_rdata_array(i, PARALLEL_IO-j-1) <= fifo_rdata(i)(DATA_WIDTH*(j+1)-1 downto DATA_WIDTH*j);
            end loop;
        end loop;
    end process;

    --------------------------------------------------------------------------  
    -- Control logic for front and back pointers, and window edges
    
    U_FRONT_CONTROL : entity work.wg_fifo_edge_control generic map (
        PARALLEL_IO     => PARALLEL_IO,
        MAX_BUFFER_SIZE => MAX_BUFFER_SIZE,
        MAX_WINDOW_ROWS => MAX_WINDOW_ROWS,
        MAX_IMAGE_COLS  => MAX_IMAGE_COLS)
        port map (
            clk             => clk,
            rst             => rst,
            en              => valid_read,
            edge            => front,
            changed         => front_changed,
            is_window_edge  => is_edge_s,
            buffer_size     => buffer_size,
            windows_per_row => std_logic_vector(windows_per_row));

    U_BACK_CONTROL : entity work.wg_fifo_edge_control generic map (
        PARALLEL_IO     => PARALLEL_IO,
        MAX_BUFFER_SIZE => MAX_BUFFER_SIZE,
        MAX_WINDOW_ROWS => MAX_WINDOW_ROWS,
        MAX_IMAGE_COLS  => MAX_IMAGE_COLS)
        port map (
            clk             => clk,
            rst             => rst,
            en              => get_next_input,
            edge            => back,
            changed         => open,
            is_window_edge  => open,
            buffer_size     => buffer_size,
            windows_per_row => std_logic_vector(windows_per_row));           

    --------------------------------------------------------------------------
    -- Logic for accessing FIFOs as a circular array

    process(front, back, fifo_full, input, input_ready, ren, fifo_read_empty, fifo_rdata_array, get_next_input, front_changed, stall_pop)
        variable any_empty       : std_logic;
        variable valid_read_temp : std_logic;

        -- initialized only to remove simulation warnings at time 0
        variable front_mod : unsigned(MAX_WINDOW_ROWS_BITS downto 0) := (others => '0');
    begin

        -- handle writes
        for i in 0 to MAX_WINDOW_ROWS-1 loop
            -- enable the wen on the current back fifo if valid_write
            if (to_unsigned(i, MAX_WINDOW_ROWS_BITS) = back) then
                if (get_next_input = '1') then
                    fifo_wen(i) <= '1';
                else
                    fifo_wen(i) <= '0';
                end if;
            else
                fifo_wen(i) <= '0';
            end if;
        end loop;

        -- "or" all fifo empty signals together
        any_empty := '0';
        for i in 0 to MAX_WINDOW_ROWS-1 loop
            any_empty := any_empty or fifo_read_empty(i);  -- changed from fifo_empty
        end loop;

        -- if any of the FIFOs are empty, the array is empty (no valid output)
        empty <= any_empty;

        -- a read is valid if ren is set and none of the fifos are empty
        valid_read_temp := ren and not any_empty;
        valid_read      <= valid_read_temp;

        -- read from all the fifos
        if (valid_read_temp = '1') then
            fifo_ren <= '1';
        else
            fifo_ren <= '0';
        end if;

        for i in 0 to MAX_WINDOW_ROWS-1 loop
            -- align the rdata outputs so front is always the first output
            front_mod := ("0"&front) + i;
            if (front_mod >= MAX_WINDOW_ROWS) then
                front_mod := front_mod - MAX_WINDOW_ROWS;
            end if;

            -- causes a harmless XST warning for non power of 2 dimensions
            -- can be fixed by making a variable that is the next higher power
            -- of 2 and setting the unused bits to 0 (Although this may cause
            -- other warnings)
            for j in 0 to PARALLEL_IO-1 loop
                output_array(i, j) <= fifo_rdata_array(to_integer(front_mod(MAX_WINDOW_ROWS_BITS-1 downto 0)), j);
            end loop;

            -- pop from the front during a valid read because that row will not
            -- be needed by any later windows. The exception is the window
            -- slides past the bottom row of the image (stall_pop='1'), in
            -- which case the circuit reuses previous rows as placeholders.
            if (valid_read_temp = '1') then
                if (i = front and stall_pop = '0') then
                    fifo_pop(i) <= '1';
                else
                    fifo_pop(i) <= '0';
                end if;
            else
                fifo_pop(i) <= '0';
            end if;

            -- if front changes, do a read reset of each fifo so that
            -- the subsequent accesses start at the beginning of each row
            if (front_changed = '1') then
                fifo_read_reset(i) <= '1';
            else
                fifo_read_reset(i) <= '0';
            end if;
        end loop;
    end process;

    --------------------------------------------------------------------------
    -- determine completion of inputs and outputs

    process (clk, rst)
    begin
        if rst = '1' then
            in_row  <= 0;
            in_col  <= 0;
            out_row <= 0;
            out_col <= 0;
        elsif (rising_edge(clk)) then

            -- in the case of new inputs, updated the row/col pointers
            if (continue = '1' and input_done = '0') then
                if (in_col >= unsigned(image_cols)-PARALLEL_IO or PARALLEL_IO > unsigned(image_cols)) then
                    in_col <= 0;
                    in_row <= in_row + 1;
                else
                    in_col <= in_col + PARALLEL_IO;
                end if;
            end if;

            -- in the case of new outputs, updated the row/col pointers
            if (valid_read = '1') then
                if (out_col >= unsigned(image_cols)-PARALLEL_IO or PARALLEL_IO > unsigned(image_cols)) then
                    out_col <= 0;
                    out_row <= out_row + 1;
                else
                    out_col <= out_col + PARALLEL_IO;
                end if;
            end if;
        end if;
    end process;

    --------------------------------------------------------------------------
    -- misc control logic

    windows_per_row <= resize(unsigned(image_cols)-unsigned(window_cols)+1, windows_per_row'length);

    -- get the next input when that input is available and the back fifo isn't
    -- full.
    -- causes a harmless XST warning for non power of 2 dimensions
    -- an be fixed by making a variable that is the next higher power of 2
    -- and setting the unused bits to 0 (Although this may cause "unused"
    -- warnings)   
    get_next_input <= input_ready and not fifo_full(to_integer(back));
    read_input     <= get_next_input;

    -- "continue" with window generation when getting the input or when there
    -- is a valid read after the entire image has been loaded, which is
    -- necessary for sliding the window past the bottom boundary of the image
    continue <= get_next_input or (valid_read and image_loaded);

-- full <= fifo_full(to_integer(back));
    -- the entire image is loaded after storing image_rows of inputs
    image_loaded <= '1' when in_row >= unsigned(image_rows) else '0';

    -- stall pops from the fifo array when the output has reached the last
    -- MAX_WINDOW_ROWS rows of the image. This is necessary because the final
    -- rows for a window smaller than the maximum size must access rows of the
    -- image that don't exist. By delaying pops from the FIFO, this allows for
    -- data in earlier rows to be reused as the extra rows.
    stall_pop <= '1' when out_row >= unsigned(image_rows) - MAX_WINDOW_ROWS and almost_done = '0' and input_done = '0' else '0';

    -- The entity is "almost done" generating windows when the window is in the
    -- last possible row of the image (image_rows-window_rows) where the window
    -- can still be fully immersed in the image. The main purpose of this signal
    -- is to clear the stall_pop flag, so that pops from the FIFO can continue.
    almost_done <= '1' when out_row >= unsigned(image_rows) - unsigned(window_rows) and input_done = '0' else '0';

    -- The entity has all of the required data to generate all windows when
    -- image_rows+MAX_WINDOW_ROWS-window_rows have been stored in the fifo
    -- array. Note that MAX_WINDOWS_ROWS-window_rows corresponds to extra rows
    -- not in the original image, which are needed to slide the maximum sized
    -- window across the additional rows needed by the smaller, actual window
    input_done <= '1' when in_row >= unsigned(image_rows) + MAX_WINDOW_ROWS - unsigned(window_rows) else '0';

    -- outputs are completely done when the output row exceeds the final row
    -- needed for the top of a window
    output_done <= '1' when out_row > unsigned(image_rows) - unsigned(window_rows) else '0';

    -- "and active" prevents done from being asserted before circuit has started
    done_s <= output_done and active;
    done   <= done_s;

    -- make sure is_edge isn't valid after done
    process(is_edge_s, done_s)
    begin
        if (done_s = '0') then
            is_edge <= is_edge_s;
        else
            is_edge <= (others => '0');
        end if;
    end process;
    
    -- convert the output array into a big vector to avoid problems with
    -- unconstrained arrays
    output <= vectorizeOutput(output_array);
    
end RTL;



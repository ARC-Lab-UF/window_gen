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

-- Description: This entity accepts a sequence of window columns and coalesces
-- them into a complete window(s). The entity implements a set of shift
-- registers equal to the size of a window. The wg_coalescer reads the next
-- columns of multiple rows, shifts the previous values over, and stores the
-- next data to generate a new window. In other words, this entity slides the
-- window by one column at a time while reusing previous data. The inputs and
-- outputs are "first-word fall through" so that the next window(s) is output
-- before ren is asserted. ren basically pops the current window(s) from the
-- buffer.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.math_custom.all;
use work.window_gen_pkg.all;

-------------------------------------------------------------------------------
-- Generics Description
-- PARALLEL_IO :  Specifies the number of input columns provided at a time,
--                which also defines the maximum number of windows output
--                from the entity.
-- NUM_ROWS : The maximum number of rows in a generated window
-- NUM_COLS : The maximum number of cols in a generated window
-- DATA_WIDTH : The width in bits of each element.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk: clock
-- rst: asynchronous reset
-- ren : '1' to pop window(s) from buffer
-- read_input : informs the source of the stream that the entity is accepting
--              data
-- empty : '1' when the entity has no valid window output, '0' otherwise
-- input : the current column(s) of input. The exact number of columns is
--         specified by the configuration option PARALLEL_IO.
-- input_ready : specifies when "input" is valid
-- input_done : '1' when all inputs have been passed to the entity
-- is_edge : '1' if the current input is the starting (left) edge/column of a
--            window. One bit for each of PARALLEL_IO input columns.
-- output : PARALLEL_IO window(s)
-- window_valid : '1' when an output window is valid, '0' otherwise. One bit
--                for each of the PARALLEL_IO windows. The next window in the
--                output sequence is always represented by the MSB. The window
--                that is PARALLEL_IO-1 windows away in the sequence is
--                represented by the LSB.
-- done : '1' when the coalescer has created all windows after input_done is
--            asserted. Note that the coalescer these windows have not
--            necessarily been read by the user yet.
-------------------------------------------------------------------------------

entity wg_coalescer is
    generic (
        PARALLEL_IO : positive;
        NUM_ROWS    : positive;
        NUM_COLS    : positive;
        DATA_WIDTH  : positive);
    port (
        clk          : in  std_logic;
        rst          : in  std_logic;
        ren          : in  std_logic;
        read_input   : out std_logic;
        empty        : out std_logic;
        input        : in  std_logic_vector(NUM_ROWS*PARALLEL_IO*DATA_WIDTH-1 downto 0);
        input_ready  : in  std_logic;
        input_done   : in  std_logic;
        is_edge      : in  std_logic_vector(PARALLEL_IO-1 downto 0);
        output       : out std_logic_vector(NUM_ROWS*(NUM_COLS+PARALLEL_IO-1)*DATA_WIDTH-1 downto 0);
        window_valid : out std_logic_vector(PARALLEL_IO-1 downto 0);        
        done         : out std_logic);
end wg_coalescer;

architecture RTL of wg_coalescer is

    -------------------------------------------------------------------------
    -- constants and types

    -- the buffer increases the number of columns to the next multiple of
    -- PARALLEL_IO to ensure that new writes don't overlap with existing data
    constant BUFFER_COLS : positive := positive(ceil(real(NUM_COLS+PARALLEL_IO-1)/real(PARALLEL_IO)))*PARALLEL_IO;

    -- The number of output columns has to account for the extra windows when
    -- PARALLEL_IO > 1
    constant OUTPUT_COLS : positive := NUM_COLS+PARALLEL_IO-1;

    -- array used to store PARALLEL_IO inputs from each row of the fifo array
    -- (i.e. window buffer)
    type input_array_t is array (0 to NUM_ROWS-1, 0 to PARALLEL_IO-1) of std_logic_vector(DATA_WIDTH-1 downto 0);

    -- 2D array that stores PARALLEL_IO windows in addition to the next
    -- PARALLEL_IO inputs
    type rbuf is array (integer range 0 to NUM_ROWS-1, integer range 0 to BUFFER_COLS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);

    -------------------------------------------------------------------------
    -- Functions for converting I/O to/from vector/array format

    -- Converts vectorized input into 2D array
    function devectorizeInput(input : std_logic_vector) return input_array_t is
        variable temp : input_array_t;
    begin
        for i in NUM_ROWS-1 downto 0 loop
            for j in 0 to PARALLEL_IO-1 loop
                temp(i, j) := getVectorElement(input, i, j, NUM_ROWS, PARALLEL_IO, DATA_WIDTH);
            end loop;
        end loop;

        return temp;
    end function;

    -- Converts register buffer into std_logic_vector used as output
    function vectorizeOutput(input : rbuf) return std_logic_vector is
        variable temp : std_logic_vector(NUM_ROWS*OUTPUT_COLS*DATA_WIDTH-1 downto 0);
    begin
        for i in 0 to NUM_ROWS-1 loop
            for j in 0 to OUTPUT_COLS-1 loop
                setVectorElement(input(i, j), temp, i, j, NUM_ROWS, OUTPUT_COLS, DATA_WIDTH);
            end loop;
        end loop;

        return temp;
    end function;

    -------------------------------------------------------------------------
    -- internal signals

    -- the register buffer
    signal regs : rbuf;

    -- asserted when there is a valid read for a window
    signal get_next_window : std_logic;

    -- asserted when an element in the row with the corresponding index is the
    -- top left element (i.e. the edge) of a generated window
    signal is_edge_s : std_logic_vector(0 to BUFFER_COLS-1);

    -- array that stores the inputs in devectorized form
    signal input_array : input_array_t;
    
begin

    --------------------------------------------------------------------
    -- Create the buffer of registers and shifting logic
    
    process(clk, rst)
    begin
        if (rst = '1') then
            for i in 0 to NUM_ROWS-1 loop
                for j in 0 to BUFFER_COLS-1 loop
                    regs(i, j) <= std_logic_vector(to_unsigned(0, DATA_WIDTH));
                end loop;
            end loop;

            for i in 0 to BUFFER_COLS-1 loop
                is_edge_s(i) <= '0';
            end loop;
            
        elsif (rising_edge(clk)) then

            if (get_next_window = '1') then

                -- shift all registers to next window
                for i in 0 to NUM_ROWS-1 loop
                    -- the top PARALLEL_IO regs get loaded directly, so don't
                    -- shift anything into them
                    for j in 0 to BUFFER_COLS-PARALLEL_IO-1 loop
                        regs(i, j) <= regs(i, j+PARALLEL_IO);
                    end loop;
                end loop;

                -- shift all edge status bits
                for j in 0 to BUFFER_COLS-PARALLEL_IO-1 loop
                    is_edge_s(j) <= is_edge_s(j+PARALLEL_IO);
                end loop;

                -- load in the new inputs
                for i in 0 to NUM_ROWS-1 loop
                    for j in 0 to PARALLEL_IO-1 loop
                        regs(i, BUFFER_COLS-PARALLEL_IO+j) <= input_array(i, j);
                    end loop;
                end loop;

                -- load in the new edge bits
                for j in 0 to PARALLEL_IO-1 loop
                    is_edge_s(j+BUFFER_COLS-PARALLEL_IO) <= input_ready and is_edge(PARALLEL_IO-j-1);
                end loop;

            end if;
        end if;
    end process;

    --------------------------------------------------------------------
    -- control logic

    -- convert input vector into 2D array
    input_array <= devectorizeInput(input);

    -- convert reg buffer into std_logic_vector output
    output <= vectorizeOutput(regs);

    -- get the next window (by shifting the buffer), but only at valid times:
    -- 1) When the user requests a read (not checked for valid)
    -- 2) When the current (0,0) element is not the edge of a window (skip it)
    -- In both cases, a new input has to be ready and the input not done
    get_next_window <= (ren or not is_edge_s(0)) and (input_ready or input_done);

    -- read an input from the fifo_array/window_buffer everytime we get a new window
    read_input <= get_next_window;

    -- empty when there is no input available (nothing to shift in) or when a
    -- window is not available
    empty <= (not input_ready and not input_done) or (not is_edge_s(0));
--    empty <= (not input_ready and not input_done) or (not is_edge_s(PARALLEL_IO-1));

    -- output valid status of PARALLEL_IO windows
    -- MSB is the first window, LSB is the last
    process(is_edge_s)
    begin
        for i in 0 to PARALLEL_IO-1 loop
            window_valid(PARALLEL_IO-i-1) <= is_edge_s(i);
        end loop;
    end process;

    -- determine when reg_buffer has output all windows
    process(is_edge_s, input_done)
    begin
        -- done when there are no more inputs and all the windows currently in
        -- the buffer are gone
        if (input_done = '1' and is_edge_s = std_logic_vector(to_unsigned(0,is_edge_s'length))) then
            done <= '1';
        else
            done <= '0';
        end if;
    end process;
    
end RTL;


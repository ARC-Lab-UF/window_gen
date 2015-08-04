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

-- Description: This entity maintains the FIFO pointers for a
-- an array of FIFOs in the window buffer. The front pointer always
-- points to the FIFO that stores the top row of window pixels, while the back
-- pointer always points to the FIFO with the bottom row. Every time the window
-- slides down one row these pointers are updated to ensure correct windows.
--
-- This entity tracks the number of times that a current row has been read in
-- order to determine when the window has reached the end of a row, which
-- determines when the pointers should be updated. The entity also determines
-- when the current reads represent the start (left edge_ of a window (is_edge).
-- For example, the generator only produces fully immersed windows, so pixels
-- towards the end of a will never form the left edge of a window.

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;
use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generics Description
-- PARALLEL_IO :  Specifies the number of inputs and outputs provided at a time,
-- DATA_WIDTH : The width in bits of each element.
-- MAX_WINDOW_ROWS : The maximum number of rows in a generated window
-- MAX_IMAGE_COLS : The maximum number of cols in an input image/stream
-- MAX_BUFFER_SIZE : The maximum size/words of each FIFO
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk: clock
-- rst: asynchronous reset
-- en : '1' to enable
-- edge : pointer to a FIFO 
-- changed : asserted when the edge changes 
-- is_window_edge : '1' if the current pointer value is the edge of a window.
--                  One bit for each PARALLEL_IO element, which the first at
--                  the MSB and the last at the LSB.
-- buffer_size : The actual size of the FIFO buffers
-- window_per_row : The number of windows generated from each image row
-------------------------------------------------------------------------------

entity wg_fifo_edge_control is
    generic (
        PARALLEL_IO     : positive;
        MAX_BUFFER_SIZE : positive;
        MAX_WINDOW_ROWS : positive;
        MAX_IMAGE_COLS  : positive);
    port (
        clk             : in  std_logic;
        rst             : in  std_logic;
        en              : in  std_logic;
        edge            : out unsigned(bitsNeeded(MAX_WINDOW_ROWS-1)-1 downto 0);
        changed         : out std_logic;
        is_window_edge  : out std_logic_vector(PARALLEL_IO-1 downto 0);
        buffer_size     : in  std_logic_vector(bitsNeeded(MAX_BUFFER_SIZE)-1 downto 0);
        windows_per_row : in  std_logic_vector(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0));
end wg_fifo_edge_control;

architecture BHV of wg_fifo_edge_control is

    signal count, next_count : integer range 0 to MAX_BUFFER_SIZE;
    signal addr, next_addr   : unsigned(bitsNeeded(MAX_WINDOW_ROWS-1)-1 downto 0);
    
begin
    
    -- registers
    process(clk, rst)
    begin
        if (rst = '1') then
            count <= 0;
            addr  <= (others => '0');
        elsif (clk = '1' and clk'event) then
            count <= next_count;
            addr  <= next_addr;
        end if;
    end process;

    -- combinational logic
    process(en, count, addr, buffer_size)
        variable temp_count : integer range 0 to MAX_BUFFER_SIZE;
    begin
        -- count inputs
        if (en = '1') then
            temp_count := count + 1;
        else
            temp_count := count;
        end if;

        -- if at the end of a row, update the edge to point to a new fifo
        if (temp_count >= unsigned(buffer_size)) then
            -- addr passes last FIFO, reset to first FIFO, otherwise increment addr
            if (addr = MAX_WINDOW_ROWS-1) then
                next_addr <= (others => '0');
            else
                next_addr <= addr + 1;
            end if;

            -- edge changes anytime an entire row has been read
            changed    <= '1';
            --temp_count := (others => '0');
            temp_count := 0;
        else
            -- if still processing a row, don't change anything
            changed   <= '0';
            next_addr <= addr;
        end if;

        next_count <= temp_count;
    end process;

    -- determine if the next PARALLEL_IO elements are the leftmost edge of a
    -- generated window
    process(count, windows_per_row)
    begin
        for i in 0 to PARALLEL_IO-1 loop
            -- each count value corresponds to PARALLEL_IO elements
            if (count*PARALLEL_IO+i < unsigned(windows_per_row)) then
                is_window_edge(PARALLEL_IO-1-i) <= '1';
            else
                is_window_edge(PARALLEL_IO-1-i) <= '0';
            end if;
        end loop;
    end process;

    edge <= addr;

end BHV;


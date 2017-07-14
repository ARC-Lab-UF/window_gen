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

-- Description: This entity maintains the FIFO used in each row of a
-- window buffer. This FIFO differs from others by having a read option and a
-- pop option, which are maintained by different pointers. The read option
-- simply scans through the FIFO contents without removing any data.
-- This functionality is needed because the window buffer will read the same
-- FIFO data multiple times before popping data out of the buffer.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generics Description
-- DATA_WIDTH : The width of each element in the FIFO.
-- ADDR_WIDTH : The address width of the FIFO. This defines the number of
--              maximum number of words in the FIFO as 2**ADDR_WIDTH.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk: Clock
-- rst: Asynchronous reset
-- wen : '1' to write current input into fifo
-- wdata : Write data
-- ren : '1' to read from the current read pointer without removing data from
--           the FIFO.
-- pop : '1' to pop data from from fifo (i.e. a traditional FIFO read)
-- read_reset : Resets read pointer to back to actual front of FIFO
-- read_empty : '1' if nothing to read
-- rdata : Data read/output from the fifo
-- empty : '1' if empty for popping
-- full : '1' if full
-- size : The size of the fifo. This can be any value from 1 to 2**ADDR_WIDTH.
--        The size is used to determine when full should be asserted.
-------------------------------------------------------------------------------

entity wg_fifo is
    generic (
        DATA_WIDTH : positive;
        ADDR_WIDTH : positive;
        SIZE_WIDTH : positive);
    port (
        clk        : in  std_logic;
        rst        : in  std_logic;
        wen        : in  std_logic;
        wdata      : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        ren        : in  std_logic;     -- doesn't decrement count
        pop        : in  std_logic;     -- decrements count
        read_reset : in  std_logic;
        read_empty : out std_logic;     -- similar to empty, but for reads
        rdata      : out std_logic_vector(DATA_WIDTH-1 downto 0);
        empty      : out std_logic;
        full       : out std_logic;
        size       : in  std_logic_vector(SIZE_WIDTH-1 downto 0)
        );
end wg_fifo;

architecture RTL of wg_fifo is

    signal front, next_front           : unsigned(ADDR_WIDTH-1 downto 0);
    signal back, next_back             : unsigned(ADDR_WIDTH-1 downto 0);
    signal read_ptr, next_read_ptr     : unsigned(ADDR_WIDTH-1 downto 0);
    signal count, next_count           : unsigned(SIZE_WIDTH-1 downto 0);
    signal read_count, next_read_count : unsigned(SIZE_WIDTH-1 downto 0);
    signal wen_ram                     : std_logic;
    signal full_s                      : std_logic;
    signal empty_s                     : std_logic;
    signal read_empty_s                : std_logic;

begin

    -- instantiate RAM uses for FIFO storage
    U_RAM : entity work.ram(SYNC_READ_DURING_WRITE)
        generic map (
            NUM_WORDS  => 2**ADDR_WIDTH,
            WORD_WIDTH => DATA_WIDTH,
            ADDR_WIDTH => ADDR_WIDTH)
        port map (
            clk   => clk,
            wen   => wen_ram,
            waddr => std_logic_vector(back),
            wdata => wdata,
            raddr => std_logic_vector(next_read_ptr),
            rdata => rdata);

    -- create necessary registers
    process(clk, rst)
    begin
        if (rst = '1') then
            front      <= (others => '0');
            back       <= (others => '0');
            read_ptr   <= (others => '0');
            count      <= (others => '0');
            read_count <= (others => '0');
        elsif(clk'event and clk = '1') then
            count      <= next_count;
            read_count <= next_read_count;
            back       <= next_back;
            front      <= next_front;
            read_ptr   <= next_read_ptr;
        end if;

    end process;

    -- combinational logic for maintaining FIFO
    process (front, back, read_ptr, count, read_count,
             wen, ren, pop, read_reset,
             full_s, read_empty_s, empty_s)
        variable temp_count      : unsigned(SIZE_WIDTH-1 downto 0);
        variable temp_read_count : unsigned(SIZE_WIDTH-1 downto 0);
    begin

        temp_count      := count;
        temp_read_count := read_count;

        -- if data is pushed in, move back and increment counts
        if (wen = '1' and full_s = '0') then
            next_back       <= back + 1;
            temp_count      := temp_count + 1;
            temp_read_count := temp_read_count + 1;
        else
            next_back <= back;
        end if;

        -- read_reset resets the read_ptr to the front pointer
        if (read_reset = '1') then
            next_read_ptr   <= front;
            temp_read_count := temp_count;
        elsif (ren = '1' and read_empty_s = '0') then
            -- if a read, advance the read pointer, but do not move front
            next_read_ptr   <= read_ptr + 1;
            temp_read_count := temp_read_count - 1;
        else
            next_read_ptr <= read_ptr;
        end if;

        -- a pop moves front and updates the count, and resets the read_ptr
        if (pop = '1' and empty_s = '0') then
            next_front      <= front + 1;
            next_read_ptr   <= front + 1;
            temp_count      := temp_count - 1;
            temp_read_count := temp_count;
        else
            next_front <= front;
        end if;

        next_count      <= temp_count;
        next_read_count <= temp_read_count;

    end process;

    full_s       <= '1' when count = unsigned(size) else '0';
    empty_s      <= '1' when count = 0              else '0';
    read_empty_s <= '1' when read_count = 0         else '0';

    full       <= full_s;
    empty      <= empty_s;
    read_empty <= read_empty_s;

    -- protect against writes when full
    wen_ram <= '1' when wen = '1' and full_s = '0' else '0';

end RTL;


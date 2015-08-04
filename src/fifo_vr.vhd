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

-- Description:
-- fifo_vr (variable read)
-- This entity implements a fifo that writes a fixed (but configurable)
-- number of inputs in parallel, while allowing a variable number of outputs
-- to be read each cycle.
--
-- The entity is useful for streams of data where upstream components always
-- produce a fixed amount of data, but downstream components may need to
-- read a variable amount of data. e.g., when streaming an image sequentially
-- from memory into an FPGA, the memory might provide four pixels per cycle.
-- If the circuit buffers each row into separate on-chip memories, then at the
-- end of a row, the circuit may need to read less than four pixels if the # of
-- columns is not a multiple of four. In this case, the buffer enables the
-- memory to continually write four pixels per cycle, while the downstream
-- circuit reads as much as needed.
--
-- The size of the fifo is fixed because its main purpose is to dynamically
-- read varying amounts of data from a stream. If a typical FIFO is needed for
-- buffering, the user should connect that FIFO to the input of this
-- entity.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;

-------------------------------------------------------------------------------
-- Generic Descriptions
-- data_width : The width of a single element to read from the FIFO
-- parallel_io : The number of parallel inputs written every cycle, which
--               is also the max number of outputs that can be read each
--               cycle.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Port Descriptions (all control signals are active high)
-- clk: clock
-- rst: asynchronous reset
-- rd : read data from the buffer (does nothing when empty is asserted)
-- rd_amount : The amount of elements to read from the buffer when rd is
--             asserted. Note that if rd_amount is larger than the actual
--             amount of elements in the fifo, the fifo returns only what
--             it has stored. e.g., If the fifo has 2 elements and rd_amount
--             is 3 when rd is asserted, the fifo will output two valid
--             elements and a third junk element.
-- wr       : write num_inputs inputs into the buffer (does nothing when
--            full is asserted)
-- empty   : asserted when the buffer is empty (has 0 elements in fifo)
-- full    : asserted when there isn't room to write num_inputs inputs.
-- input   : num_inputs input values, concatenated into a big std_logic_vector
-- output  : max_outputs outputs. When rd_amount < max_outputs, the higher bits
--            contains valid datas.
-- count     : the number of valid data_width elements in the fifo
-- valid_out : individual valid bits for each output
-------------------------------------------------------------------------------

entity fifo_vr is
    generic (
        data_width     : positive;
        parallel_io    : positive;
        input0_at_MSB  : boolean := false;
        output0_at_MSB : boolean := false);
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;
        rd        : in  std_logic;
        rd_amount : in  std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
        wr        : in  std_logic;
        empty     : out std_logic;
        full      : out std_logic;
        input     : in  std_logic_vector(parallel_io*data_width-1 downto 0);
        output    : out std_logic_vector(parallel_io*data_width-1 downto 0);
        count     : out std_logic_vector(bitsNeeded(parallel_io)-1 downto 0);
        valid_out : out std_logic_vector(parallel_io-1 downto 0));
end fifo_vr;

architecture DEFAULT of fifo_vr is

    type data_array is array (natural range <>) of std_logic_vector(data_width-1 downto 0);
    signal count_s      : unsigned(bitsNeeded(parallel_io)-1 downto 0);
    signal front        : integer range 0 to parallel_io-1;
    signal regs         : data_array(0 to parallel_io*2-1);
    signal valid_wr     : std_logic;
    signal valid_rd     : std_logic;
    signal full_s       : std_logic;
    signal empty_s      : std_logic;
    signal valid        : std_logic_vector(0 to parallel_io*2-1);
    signal valid_out_s  : std_logic_vector(0 to parallel_io-1);
    signal inputs       : data_array(0 to parallel_io-1);
    signal window_reset : std_logic;
    signal rd_amount_s  : integer range 0 to parallel_io;

begin

    -- the buffer is empty any of the valid bits in the lower half are '0'
    empty_s <= not valid(0);
    empty   <= empty_s;

    -- the buffer is full when any of the valid bits in the upper half are
    -- asserted, but not when there is a valid read that resets the window
    full_s <= valid(parallel_io) and not (valid_rd and window_reset) and
              not empty_s;
    full <= full_s;

    -- check for valid rd/wr to avoid data loss
    valid_wr <= wr and not full_s;
    valid_rd <= rd and not empty_s;

    count     <= std_logic_vector(count_s);
    valid_out <= valid_out_s;

    -- the window position is reset any time front extends past the first half
    window_reset <= '0' when front + rd_amount_s < parallel_io else '1';

    -- devectorize the input vector into an array based on the whether or not
    -- the first input is at the MSB or LDB
    U_INPUT0_AT_MSB : if (INPUT0_AT_MSB) generate
        process(input)
        begin
            for i in 0 to parallel_io-1 loop
                inputs(i) <= input(input'length-i*data_width-1 downto input'length-(i+1)*data_width);
            end loop;
        end process;
    end generate;

    U_INPUT0_AT_LSB : if (INPUT0_AT_MSB = false) generate
        process(input)
        begin
            for i in 0 to parallel_io-1 loop
                inputs(i) <= input((i+1)*data_width-1 downto i*data_width);
            end loop;
        end process;
    end generate;

    -- calculate the number of valid bits in the output
    process(valid_out_s)
        variable temp : unsigned(bitsNeeded(parallel_io)-1 downto 0);
    begin

        -- calculate the number of valid outputs
        temp := (others => '0');
        for i in 0 to parallel_io-1 loop
            if (valid_out_s(i) = '1') then
                temp := temp + 1;
            end if;
        end loop;

        count_s <= temp;
    end process;

    -- make sure rd_amount can't exceed number of valid outputs
    process(rd_amount, count_s)
    begin
        if (unsigned(rd_amount) > count_s) then
            rd_amount_s <= to_integer(unsigned(count_s));
        else
            rd_amount_s <= to_integer(unsigned(rd_amount));
        end if;
    end process;

    -- align the output with a parallel_io-element window starting at front
    process(regs, front, valid)
        variable index : integer range 0 to parallel_io*2-2;
    begin
        for i in 0 to parallel_io-1 loop

            -- no need for mod because window gets reset to avoid extending
            -- past end of the buffer
            index := front + i;

            -- set the corresponding output bits for the current register
            if (OUTPUT0_AT_MSB) then
                -- the first output starts at the highest index, whereas
                -- the stored register values start at the lowest index
                output(output'length-data_width*i-1 downto output'length-data_width*(i+1)) <= regs(index);

                -- align the output valid bits;
                valid_out_s(i) <= valid(index);
            else
                output((i+1)*data_width-1 downto i*data_width) <= regs(index);
                valid_out_s(parallel_io-i-1) <= valid(index);
            end if;

        end loop;
    end process;

    process(clk, rst)
    begin
        if (rst = '1') then

            for i in 0 to parallel_io*2-1 loop
                regs(i)  <= (others => '0');
                valid(i) <= '0';
            end loop;

            front <= 0;

        elsif (rising_edge(clk)) then

            -- during a read, slide the front of the window so the next output
            -- is aligned properly. 
            if (valid_rd = '1') then
                if (window_reset = '0') then
                    front <= front + rd_amount_s;
                else
                    front <= front + rd_amount_s - parallel_io;
                end if;
            end if;

            -- check if the bottom half is empty. All parallel_io valid bits
            -- of each half should be the same, so only the first bit has to be
            -- checked
            if (valid(0) = '0' or
                (valid_rd = '1' and window_reset = '1')) then

                -- move the top half to the bottom.
                for i in 0 to parallel_io-1 loop
                    regs(i)  <= regs(i+parallel_io);
                    valid(i) <= valid(i+parallel_io);
                end loop;

                -- reset the valid bits for the top half
                for i in parallel_io to parallel_io*2-1 loop
                    valid(i) <= '0';
                end loop;
            end if;

            -- write new data to the top half of buffer
            if (valid_wr = '1') then
                for i in 0 to parallel_io-1 loop
                    regs(i+parallel_io)  <= inputs(i);
                    valid(i+parallel_io) <= '1';
                end loop;
            end if;

        end if;
    end process;

end DEFAULT;


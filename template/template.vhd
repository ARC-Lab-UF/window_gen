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

-- This file provides a template for integrating the window generator into
-- a datapath. Note that this is not a functional example and is instead
-- used solely for explaining how to interface an input source with the
-- generator, and how to handle outputs from the generator.
--
-- The code has various TODO markers that should be updated when using a
-- specific application.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.math_custom.all;
use work.window_gen_pkg.all;


entity template is
    generic (
        IN_RAM_WORDS   : positive;
        OUT_RAM_WORDS  : positive;
        IN_DATA_WIDTH  : positive;
        OUT_DATA_WIDTH : positive;

        MAX_WINDOW_ROWS : positive;
        MAX_WINDOW_COLS : positive;
        MAX_IMAGE_ROWS  : positive;
        MAX_IMAGE_COLS  : positive;

        NUM_PIPELINES : positive);
    port (
        clk : in std_logic;
        rst : in std_logic;
        go  : in std_logic;

        window_rows : in std_logic_vector(bitsNeeded(MAX_WINDOW_ROWS)-1 downto 0);
        window_cols : in std_logic_vector(bitsNeeded(MAX_WINDOW_COLS)-1 downto 0);

        image_cols : in std_logic_vector(bitsNeeded(MAX_IMAGE_COLS)-1 downto 0);
        image_rows : in std_logic_vector(bitsNeeded(MAX_IMAGE_ROWS)-1 downto 0)

     -- TODO: add application specific I/O
        );
end template;

architecture default of template is

    constant WINDOW_BITS : positive := MAX_WINDOW_ROWS*MAX_WINDOW_COLS*IN_DATA_WIDTH;
    -- 2D array representing a window
    type window_t is array(0 to MAX_WINDOW_ROWS-1, 0 to MAX_WINDOW_COLS-1) of std_logic_vector(IN_DATA_WIDTH-1 downto 0);
    -- 1D array of 2D windows 
    type window_array_t is array (0 to NUM_PIPELINES-1) of window_t;

    -- a huge vectorized version of all NUM_PIPELINE windows
    signal wg_out : std_logic_vector(MAX_WINDOW_ROWS*(MAX_WINDOW_COLS+NUM_PIPELINES-1)*IN_DATA_WIDTH-1 downto 0);

    -- all of the windows in 2D format
    signal windows : window_array_t;

    signal window_valid : std_logic_vector(NUM_PIPELINES-1 downto 0);
    signal wg_done      : std_logic;
    signal wg_empty     : std_logic;
    signal wg_ready     : std_logic;
    signal read_windows : std_logic;
    signal input_valid  : std_logic;

    signal pipe_valid_in : std_logic_vector(NUM_PIPELINES-1 downto 0);

    signal in_ram_wen   : std_logic;
    signal in_ram_waddr : std_logic_vector(bitsNeeded(IN_RAM_WORDS)-1 downto 0);
    signal in_ram_wdata : std_logic_vector(IN_DATA_WIDTH-1 downto 0);
    signal in_ram_raddr : std_logic_vector(bitsNeeded(IN_RAM_WORDS)-1 downto 0);
    signal in_ram_rdata : std_logic_vector(IN_DATA_WIDTH-1 downto 0);

    signal out_ram_wen   : std_logic;
    signal out_ram_waddr : std_logic_vector(bitsNeeded(OUT_RAM_WORDS)-1 downto 0);
    signal out_ram_wdata : std_logic_vector(OUT_DATA_WIDTH-1 downto 0);
    signal out_ram_raddr : std_logic_vector(bitsNeeded(OUT_RAM_WORDS)-1 downto 0);
    signal out_ram_rdata : std_logic_vector(OUT_DATA_WIDTH-1 downto 0);

    type state_t is (WAIT_FOR_GO, GENERATE_ADDRESS);
    signal state : state_t;

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
    -- window : the window as a 2D array (t_window)
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
                window(i, j-index) := getVectorElement(vector, i, j, MAX_WINDOW_ROWS, MAX_WINDOW_COLS+NUM_PIPELINES-1, IN_DATA_WIDTH);
            end loop;
        end loop;
    end devectorizeWindow;

begin  -- STR

    -------------------------------------------------------------------------
    -- Input Source
    -- Notes: Inputs can be provided to the sliding-window generator from
    -- potentially any source. In this example, we instantiate a block RAM with
    -- a word width equivalent to the number of replicated pipelines we would
    -- like to use. For example, if NUM_PIPELINES is 4, this RAM can provide 4
    -- inputs to the window generator every cycle, which will later output up
    -- to 4 windows per cycle.
    --
    -- If using an external RAM as the input source, this RAM would likely be
    -- replaced with a FIFO that has a read port width of the same size as this
    -- RAM word width. In most cases, that FIFO will have a write port width
    -- equivalent to the width of the external memory's data bus. For example,
    -- if reading from a memory with a 128-bit data bus into a window generator
    -- that reads 64 bits per cycle, you would use a FIFO with a 128-bit write
    -- port and 64-bit read port.
    --
    -- A third possibility for the input source is to connect the window
    -- generator directly to the output of another entity. e.g. a chain
    -- of filters, where each filter uses a sliding-window generator followed
    -- by a pipeline that provides input to the next window generator.
    -------------------------------------------------------------------------

    U_INPUT_RAM : entity work.RAM(SYNC_READ)
        generic map(
            num_words  => IN_RAM_WORDS,
            word_width => IN_DATA_WIDTH*NUM_PIPELINES,
            addr_width => bitsNeeded(IN_RAM_WORDS))
        port map (
            clk   => clk,
            wen   => in_ram_wen,
            waddr => in_ram_waddr,
            wdata => in_ram_wdata,
            raddr => in_ram_raddr,
            rdata => in_ram_rdata);

    -- example addressing logic for the input RAM
    -- The exact logic depends on the type input source, the timing of the
    -- input source, etc. This example is simply meant to illustrate the 
    -- basic concept for generating an input stream for the for the window
    -- generator.

    process(clk, rst)
    begin
        if (rst = '1') then

            in_ram_raddr <= (others => '0');
            input_valid  <= '0';
            state        <= WAIT_FOR_GO;

        elsif (rising_edge(clk)) then

            case state is
                when WAIT_FOR_GO =>

                    in_ram_raddr <= (others => '0');
                    input_valid  <= '0';

                    if (go = '1') then
                        state <= GENERATE_ADDRESS;
                    end if;

                when GENERATE_ADDRESS =>


                    if (wg_ready = '1') then
                        in_ram_raddr <= std_logic_vector(unsigned(in_ram_raddr) + 1);
                        -- TODO: Update timing depending on latency of input source
                        input_valid  <= '1';
                    end if;

                    -- TODO: Add completion logic

                when others => null;

            end case;

        end if;
    end process;

    -------------------------------------------------------------------------
    -- Sliding-window generator
    -------------------------------------------------------------------------

    U_WINDOW_GEN : entity work.window_gen
        generic map (
            PARALLEL_IO     => NUM_PIPELINES,
            MAX_WINDOW_ROWS => MAX_WINDOW_ROWS,
            MAX_WINDOW_COLS => MAX_WINDOW_COLS,
            MAX_IMAGE_ROWS  => MAX_IMAGE_ROWS,
            MAX_IMAGE_COLS  => MAX_IMAGE_COLS,
            INPUT0_AT_MSB   => false,   -- RAM likely stores first input at LSB
            DATA_WIDTH      => IN_DATA_WIDTH)
        port map (
            clk          => clk,
            rst          => rst,
            go           => go,
            ready        => wg_ready,
            read_enable  => read_windows,
            empty        => wg_empty,
            image_rows   => image_rows,
            image_cols   => image_cols,
            window_rows  => window_rows,
            window_cols  => window_cols,
            input        => in_ram_rdata,  -- input from RAM
            input_valid  => input_valid,  -- TODO: Make sure this logic is timed
                                          -- appropriately.

            output       => wg_out,
            window_valid => window_valid,
            done         => wg_done
            );

    -- The output of the window buffer is a huge std_logic_vector that
    -- represents all NUM_PIPELINES windows. This code converts the huge vector
    -- into an 2D array representation that is easier to work with.
    --
    -- This would be made much easier by VHDL 2008, where the generator could
    -- output this array itself. Once 2008 is more widely supported, we plan to
    -- add a 2008 wrapper around the window generator to provide a more
    -- convenient interface.
    process(wg_out)
        variable temp_window : window_t;
    begin
        for i in 0 to NUM_PIPELINES-1 loop
            devectorizeWindow(wg_out, temp_window, i);
            windows(i) <= temp_window;
        end loop;
    end process;

    -- read/remove the current windows when the generator isn't empty
    -- TODO: add pipeline enable here if necessary (e.g. likely shouldn't read
    -- if the pipeline is stalled)
    read_windows <= not wg_empty;

    -------------------------------------------------------------------------
    -- Replicated Pipelines
    -- TODO: Add sliding-window function here for each window.
    -------------------------------------------------------------------------

    U_PIPELINES : for i in 0 to NUM_PIPELINES-1 generate

        -- determine the validity of each pipeline input (i.e. window)
        -- TODO: add pipeline enable here if necessary (e.g., if the window
        -- wasn't read from the generator, you might not want to tell the
        -- pipeline the input is valid)
        pipe_valid_in(i) <= window_valid(i);

        -- TODO: INSTANTIATE SLIDING-WINDOW FUNCTION PIPELINES
        -- FOR EACH WIDOW window(i)

    end generate;

    -------------------------------------------------------------------------
    -- Output
    -- Notes: Like the input source, the output can potentially be any circuit
    -- that can accept a stream of pipeline outputs.
    --
    -- One additional challenge for storing outputs is that there are cases
    -- where the window generator will output less than NUM_PIPELINES windows,
    -- in which case some of the pipeline outputs will be invalid while others
    -- are valid.
    --
    -- To deal with this situation, we recommend several possibilities. One
    -- simple solution if storing to memory is to also store the invalid
    -- outputs and then use softwareto filter out the invalid outputs. 
    -- A second option is to use a FIFO-like entity that can accept a 
    -- variable number of inputs (i.e. pipeline outputs) every cycle. 
    -- In this case, the FIFO will only store the valid outputs,
    -- which filters out the invalid outputs. If connecting the pipeline
    -- outputs to another downstream entity, it may also be convenient to 
    -- filter out the invalid outputs there.
    -------------------------------------------------------------------------

    U_OUTPUT_RAM : entity work.RAM(SYNC_READ)
        generic map(
            num_words  => OUT_RAM_WORDS,
            word_width => OUT_DATA_WIDTH,
            addr_width => bitsNeeded(OUT_RAM_WORDS))
        port map (
            clk   => clk,
            wen   => out_ram_wen,
            waddr => out_ram_waddr,
            wdata => out_ram_wdata,
            raddr => out_ram_raddr,
            rdata => out_ram_rdata);

end default;





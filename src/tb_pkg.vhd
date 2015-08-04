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
-- Yang Zheng
-- Eric Schwartz
-- University of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;
--use ieee.fixed_float_types.all;
--use ieee.float_pkg.all;        

--library ieee_proposed;
--use ieee_proposed.fixed_float_types.all;  -- ieee in the release 
--use ieee_proposed.float_pkg.all;          -- ieee.float_pkg.all; in the release

package tb_pkg is

    -----------------------------------------------------------------------
    -- Procedure randomIn
    -- Description: create random integer within a specified range
    --
    -- Parameters:
    -- seed1/2  : seed values for random number generation
    -- min : lower bound on randomly generated number
    -- max : upper bound on randomly generated number
    -- result : randomly generated integer
    --
    -- Preconditions: min <= max
    -----------------------------------------------------------------------
    procedure randomInt
        (variable seed1, seed2 : inout positive; min, max : in integer;
         variable result       : out   integer);

    ----------------------------------------------------------
    -- Procedure randDecision
    -- Description: randomly decide true/false based on a specified probability
    --
    -- Parameters:
    -- seed1/2  : seed values for random number generation
    -- prob     : probability that decision will be yes/true
    -- decision : the resulting decision
    ----------------------------------------------------------
    procedure randDecision(seed1, seed2 : inout positive;
                           prob         :       real;
                           decision     : out   boolean);

    ----------------------------------------------------------
    -- Procedure randDelay
    -- Description: Create a delay with random cycle length, with specified
    --              probability.
    --
    -- Parameters:
    -- seed1/2  : seed values for random number generation
    -- clk      : The clock signal used for the delay
    -- prob     : probability that the delay will occur
    -- min      : the minimum cycle delay
    -- max      : the maximum cycle delay
    ----------------------------------------------------------
    procedure randDelay(seed1, seed2 : inout positive;
                        signal clk   :       std_logic;
                        prob         :       real;
                        min, max     :       natural);

end tb_pkg;

package body tb_pkg is

--     procedure randomFloat(variable seed1, seed2 : inout positive; min, max : in real; variable result : out std_logic_vector(31 downto 0)) is
--         variable rand        : real;    -- Random real value in range 0 to 1.0
--         variable result_real : float32;
--     begin
--         assert (min <= max) report "ERROR: In randomFloat(), min must be <= max" severity error;
        
--         UNIFORM(seed1, seed2, rand);    -- generate random number  
--         --report"Random value is" & real'image(rand);
--         result_real := to_float((max-min)*rand+min, result_real'high, -result_real'low);
--         result      := to_slv(result_real);
--     end randomFloat;


    procedure randomInt(variable seed1, seed2 : inout positive; min, max : in integer; variable result : out integer) is
        variable rand : real;           -- Random real value in range 0 to 1.0
    begin
        assert (min <= max) report "ERROR: In randomInt(), min must be <= max" severity error;

        UNIFORM(seed1, seed2, rand);    -- generate random number  
        result := integer(TRUNC(real(max-min)*rand+real(min)));
    --report"Random(int) value is " & integer'image(result);
    end randomInt;

    
    procedure randDecision(seed1, seed2 : inout positive;
                           prob         :       real;
                           decision     : out   boolean) is
        variable rand : real;
    begin
        
        UNIFORM(seed1, seed2, rand);
        if rand < prob then
            decision := true;
        else
            decision := false;
        end if;
    end procedure;

    procedure randDelay(seed1, seed2 : inout positive;
                        signal clk   :       std_logic;
                        prob         :       real;
                        min, max     :       natural) is

        variable should_delay : boolean;
        variable cycle_delay  : natural;
    begin

        -- decide whether or not to delay
        randDecision(seed1, seed2, prob, should_delay);

        if should_delay then
            -- delay by a random amount between min and max
            randomInt(seed1, seed2, min, max, cycle_delay);
            if cycle_delay > 0 then
                for i in 0 to cycle_delay-1 loop
                    wait until rising_edge(clk);
                end loop;
            end if;
        end if;
    end procedure;

end package body;


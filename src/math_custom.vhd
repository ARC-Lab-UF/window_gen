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

-- This file contains custom math functions.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package math_custom is

    -- Function: clog2()
    -- Description: Takes the ceil(log2) of a positive integer.
    -- Useful for avoiding all the casting that takes place in math_real.
    function clog2(input : positive) return natural;

    -- Function: clog2_pos()
    -- Description: Computes ceil(log2(input)), but with clog2(1)=1 instead of 0
    -- Useful for getting minimum bits needed to index "input" number of
    -- distinct values, such as for addresses.
    function clog2_pos(input : positive) return positive;

    -- Function: bitsNeeded
    -- Description: returns the number of bits required to store a specified
    --              value. Can also be used to determine the bits required for
    --              a specific number of unique values by substracting one from
    --              the inputs.
    -- e.g. 1, when trying to determine how many bits are needed to store a
    -- value X, call bitsNeeded(X)
    --
    -- e.g. 2, when trying to determine how many bits are needed for an address
    -- space with X elements, call bitsNeeded(X-1). For example, an address
    -- space for 8 elements would require bitsNeeded(7) because 7 would be the
    -- highest address.
    function bitsNeeded(value : positive) return positive;


    -- Function: bitsForValue
    -- Description: An alias for bitsNeeded to provide a more consistent naming
    --              convention that matches bitsForAmount()
    function bitsForValue(value : positive) return positive;
    
    -- Function: bitsForAmount
    -- Description: returns the number of bits required to store "amount"
    --              distinct values. This is useful when determining the number
    --              of bits for an address space. It is also equivalent to
    --              bitsNeeded(amount-1).
    -- e.g. 1, when trying to determine how many bits are needed for an address
    -- space with X elements, call bitsForAmount(X). For example, an address
    -- space for 8 elements would require bitsForAmount(8) bits. Alternatively,
    -- bitsNeeded(8-1) also works.
    function bitsForAmount(amount : positive) return positive;
    
end package;

package body math_custom is

    function clog2(input : positive) return natural is
    begin
        return natural(ceil(log2(real(input))));
    end function;

    function clog2_pos(input : positive) return positive is
        variable temp   : natural;
        variable logVal : natural;
    begin
        if (input = 1) then
            return 1;
        else
            return positive(ceil(log2(real(input))));
        end if;
    end function;

    function bitsNeeded(value : positive) return positive is
    begin
        return clog2_pos(value+1);
    end function;

    function bitsForValue(value : positive) return positive is
    begin
        return bitsNeeded(value);
    end function;

    function bitsForAmount(amount : positive) return positive is
    begin
        return clog2_pos(amount);
    end function;    

end package body;


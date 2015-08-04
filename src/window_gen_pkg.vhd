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

-- This package provides helper functions for accessing windows that have been
-- represented as a large std_logic_vector.
--
-- NOTE: This package will become largely unnecessary after using VHDL 2008.
-- VHDL 93 does not support arrays of unconstrained types, so creating a
-- completely generic window is not possible without representing it as a huge
-- std_logic_vector. In 2008, that vector would be replaced by a 2D array of an
-- unconstrained width. The width would be specified by the DATA_WIDTH
-- parameter when the window generator is instantiated.

library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

package window_gen_pkg is

    -----------------------------------------------------------------------
    -- Function getVectorElement
    -- Description: For a vectorized 2D array, this function returns the
    --              element at the specified row and col.
    --
    -- Parameters:
    -- vector : The 1D-vectorized version of the 2D array, stored in row-major
    --          order. Index (0,0) starts at the MSB in the vector, with the
    --          LSB storing the end of index (total_rows-1, total_cols-1)
    -- row : the row of the element to return
    -- col : the column of the element to return     
    -- total_rows : the total number of rows in the 2D array (and 1D vectorized
    --              version)
    -- total_cols : the total number of cols in the 2D array (and 1D vectorized
    --              version)
    -- element_width : the width in bits of each array element

    -- Preconditions: row < total_rows, col < total_cols, vector must be the
    -- appropriate size with data stored as described above.
    -----------------------------------------------------------------------
    function getVectorElement(vector                 : std_logic_vector;
                              row, col               : natural;
                              total_rows, total_cols : positive;
                              element_width          : positive) return std_logic_vector;


    -----------------------------------------------------------------------
    -- Function setVectorElement
    -- Description: For a vectorized 2D array, this function set the
    --              element at the specified row and col with the specified
    --              value.
    --
    -- Parameters:
    -- input : The element-width value to store into the window.
    -- vector : The 1D-vectorized version of the 2D array, stored in row-major
    --          order. Index (0,0) starts at the MSB in the vector, with the
    --          LSB storing the end of index (total_rows-1, total_cols-1).
    --          The procedure takes the vector as input and modifies the
    --          corresponding element.
    -- row : the row of the element to set
    -- col : the column of the element to set     
    -- total_rows : the total number of rows in the 2D array (and 1D vectorized
    --              version)
    -- total_cols : the total number of cols in the 2D array (and 1D vectorized
    --              version)
    -- element_width : the width in bits of each array element

    -- Preconditions: row < total_rows, col < total_cols, vector must be the
    -- appropriate size as described above.
    -----------------------------------------------------------------------
    procedure setVectorElement(input                  :       std_logic_vector;
                               vector                 : inout std_logic_vector;
                               row, col               :       natural;
                               total_rows, total_cols :       positive;
                               element_width          :       positive);

end window_gen_pkg;


package body window_gen_pkg is


    function getVectorElement(vector                 : std_logic_vector;
                              row, col               : natural;
                              total_rows, total_cols : positive;
                              element_width          : positive) return std_logic_vector is

        variable top : natural;
    begin

        assert(row < total_rows);
        assert(col < total_cols);

        top := ((total_rows-row) * total_cols - col) * element_width;
        return vector(top-1 downto top-element_width);
    end getVectorElement;

    procedure setVectorElement(input                  :       std_logic_vector;
                               vector                 : inout std_logic_vector;
                               row, col               :       natural;
                               total_rows, total_cols :       positive;
                               element_width          :       positive) is

        variable top : natural;
    begin

        assert(row < total_rows);
        assert(col < total_cols);

        top                                    := ((total_rows-row) * total_cols - col) * element_width;
        vector(top-1 downto top-element_width) := input;
    end setVectorElement;

end package body;


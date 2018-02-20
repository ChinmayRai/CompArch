library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.CONV_STD_LOGIC_VECTOR;
use ieee.NUMERIC_STD.all;

entity shifter is
    Port (
    A           : in  STD_LOGIC_VECTOR(31 downto 0);  -- 1 inputs 32-bit
    shift_type  : in std_logic_vector(1 downto 0);
    shiftamt    : in STD_LOGIC_VECTOR(4 downto 0);  -- input 5-bit for selecting function
    ALU_out     : out STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    shift_carry : out std_logic --shiter carry
    );
end shifter; 

architecture Behavioral of shifter is
signal ALU_Result : std_logic_vector (31 downto 0);
signal carry      : std_logic;
signal amt        : integer;

begin
amt <= to_integer(unsigned(shiftamt));
WITH shift_type SELECT
ALU_Result <= STD_LOGIC_VECTOR(shift_left(unsigned(A),amt)) WHEN "00",
              STD_LOGIC_VECTOR(shift_right(unsigned(A),amt)) WHEN "01",
              STD_LOGIC_VECTOR(shift_right(signed(A), amt)) WHEN "10",
              STD_LOGIC_VECTOR(rotate_right(signed(A),amt)) WHEN OTHERS;
       
WITH shift_type SELECT           
carry <= A(32-amt) WHEN "00",
         A(amt-1) WHEN OTHERS ;
   
 ALU_out <= ALU_Result;
end Behavioral;
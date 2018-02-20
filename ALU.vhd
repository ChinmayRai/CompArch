library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity ALU is
    Port (
    A, B     : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    carry    : in std_logic_vector(31 downto 0);
    ALU_Sel  : in  STD_LOGIC_VECTOR(3 downto 0);  -- input 4-bit for selecting function
    ALU_out  : out  STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    flag     : out std_logic_vector(3 downto 0)        -- CNZV flags
    );
end ALU; 

architecture Behavioral of ALU is

signal ALU_Result : std_logic_vector (31 downto 0);
signal c31,c32: std_logic;

begin
   process(A,B,ALU_Sel)
 begin
  case(ALU_Sel) is
  when "0000" => -- Logical AND
   ALU_Result <= A and B ; 
  when "0001" => -- Logical XOR
   ALU_Result <= A xor B ;
  when "0010" => -- Substraction
   ALU_Result <= A + (Not B) + 1 ;
  when "0011" => -- Reverse Subtract
   ALU_Result <=  (Not A) + B + 1 ;
  when "0100" => -- Additon
   ALU_Result <= A + B ;
  when "0101" => -- Addition with carry
   ALU_Result <= A + B + carry ;
  when "0110" => -- Substraction with carry
   ALU_Result <= A + (NOT B) + carry ;
  when "0111" => -- Reverse Subtract with Carryout
   ALU_Result <=  (Not A) + B + carry ;
  when "1000" => -- test
   ALU_Result <= A and B;
  when "1001" => -- test eq
   ALU_Result <= A xor B;
  when "1010" => -- cmp
   ALU_Result <= A + (Not B) + 1 ;
  when "1011" => -- cmn
   ALU_Result <= A + B;
  when "1100" => -- Logical Or
   ALU_Result <= A or B;
  when "1101" => -- mov
   ALU_Result <= B ;
  when "1110" => -- bit clear
   ALU_Result <= A and (NOT B);
  when others => -- mvn 
   ALU_Result <= Not B;                
  end case;
 end process;

 flag(2) <= ALU_Result(31);
 WITH ALU_Result SELECT
 flag(1)<= '1' WHEN "00000000000000000000000000000000",
           '0' WHEN OTHERS;

c31 <= A(31) xor (B(31) xor ALU_Result(31));
c32 <= (A(31) and B(31)) or ((A(31) and c31) or(B(31) and c31));

flag(0) <= c31 xor c32;
flag(3) <= c32;

ALU_Out <= ALU_Result; -- ALU out
end Behavioral;

















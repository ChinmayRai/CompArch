library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity chull_register is
	Port(
		write_data : in std_logic_vector(31 downto 0); -- data to be written
		read_add1,read_add2 : in std_logic_vector(3 downto 0); -- 4-bit read address for read ports
		write_add  : in std_logic_vector(3 downto 0); -- 4-bit address for writing
		clock, reset, write_enable : in std_logic;
		read_data1,read_data2  : out std_logic_vector(31 downto 0)
	); 
end chull_register;

architecture Behavioral of chull_register is
type chull_register is array (0 to 15) of std_logic_vector(31 downto 0); 
signal reg : chull_register;
signal indexr1,indexr2,indexw : integer;

begin
indexr1 <= to_integer(unsigned(read_add1));
indexr2 <= to_integer(unsigned(read_add2));
indexw  <= to_integer(unsigned(write_add));
read_data1 <= reg(indexr1);
read_data2 <= reg(indexr2);

	process(clock,reset)
	begin
	if (reset = '1')then
    reg(15)<="00000000000000000000000000000000";	
	else if (clock = '1' and clock'event) then
    	if write_enable = '1' then
    		reg(indexw) <= write_data;
		end if;		
	end if ;
	end if;

	end process;

end Behavioral;
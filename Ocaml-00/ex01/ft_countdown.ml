let ft_countdown start =
  if start < 0 then (
    print_int 0;
    print_char '\n'
  ) else
    for count = start downto 0 do
      print_int count;
      print_char '\n'
    done
NAME = ex02
SRC = atom.ml molecule.ml alkane.ml main.ml

all: $(NAME)

$(NAME): $(SRC)
	ocamlopt -o $(NAME) $(SRC)

clean:
	rm -f *.cmi *.cmx *.o

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
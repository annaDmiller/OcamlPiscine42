NAME = ex00
SRC = atom.ml main.ml

all: $(NAME)

$(NAME): $(SRC)
	ocamlopt -o $(NAME) $(SRC)

clean:
	rm -f *.cmi *.cmx *.o

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
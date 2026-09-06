NAME = ex01

all: $(NAME)

$(NAME): people.ml doctor.ml main.ml
	ocamlopt -o $(NAME) people.ml doctor.ml main.ml

clean:
	rm -f *.cmi *.cmx *.o

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re

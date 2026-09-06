NAME = ex02

all: $(NAME)

$(NAME): people.ml doctor.ml dalek.ml main.ml
	ocamlopt -o $(NAME) people.ml doctor.ml dalek.ml main.ml

clean:
	rm -f *.cmi *.cmx *.o

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re

NAME = watchtower

SRCS = main.ml

$(NAME) : all

all: $(SRCS)
	ocamlopt -o $(NAME) $(SRCS)

clean:
	rm -f *.o *.cmx *.cmi

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
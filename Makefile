##
## EPITECH PROJECT, 2024
## Pandoc
## File description:
## This is just a Makefile, but v5
##

NAME = mypandoc
LOCATION = $(shell stack path --local-install-root)
TIX_LOCATION = $(shell stack path --local-hpc-root)

ANSI_RED = \033[0;31m
ANSI_GREEN = \033[0;32m
ANSI_YELLOW = \033[0;33m
ANSI_BLUE = \033[0;34m
ANSI_PURPLE = \033[0;35m
ANSI_RESET = \033[0m

all: $(NAME)

$(NAME):
	@echo "$(ANSI_BLUE)>> Building $(NAME)$(ANSI_RESET)"
	@stack build
	@echo "$(ANSI_GREEN)>> Built $(NAME)$(ANSI_RESET)"
	@cp $(LOCATION)/bin/$(NAME)-exe $(NAME)

clean:
	@echo "$(ANSI_YELLOW)>> Cleaning $(NAME)$(ANSI_RESET)"
	@stack clean

fclean: clean
	@echo "$(ANSI_RED)>> Removing $(NAME)$(ANSI_RESET)"
	@rm -rf $(NAME)

re: fclean all

.PHONY: all clean fclean re

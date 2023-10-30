:- use_module(library(lists)).

:- consult('board.pl').
:- consult('menu.pl').
:- consult('utils.pl').
:- consult('inputs.pl').
:- consult('game.pl').

% Main function
play :-
    game_start(5, GameState).
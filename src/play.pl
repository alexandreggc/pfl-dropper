:- use_module(library(lists)).
:- use_module(library(between)).

:- consult('board.pl').
:- consult('menu.pl').
:- consult('utils.pl').
:- consult('inputs.pl').
:- consult('logic.pl').
:- consult('game.pl').
:- consult('graph.pl').

% Main function
play :-
    game_start(3, GameState).
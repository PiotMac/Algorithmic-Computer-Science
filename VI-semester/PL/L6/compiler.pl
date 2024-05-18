wykonaj(NazwaPliku) :-
    open(NazwaPliku, read, Strumien),
    scanner(Strumien, Tokeny),
    close(Strumien),
    filter_end_of_file(Tokeny, FilteredTokens),
    phrase(program(Program), FilteredTokens),
    interpreter(Program).


% ---------- LEXER ----------
separators([';', '*', '/', '+', '-', '=', '/=', '<', '>', '>=', '=<', '(', ')', ':=']).
keywords(['read', 'write', 'if', 'then', 'else', 'fi', 'while', 'do', 'od', 'mod', 'or', 'and']).


scanner(Strumień, Tokeny) :-
    get_char(Strumień, Znak),
    scanner(Znak, Strumień, Tokeny).

scanner(end_of_file, _, []) :- !.

scanner(Znak, Strumień, [Token|Tokeny]) :-
    char_type(Znak, space), !,
    get_char(Strumień, NastępnyZnak),
    scanner(NastępnyZnak, Strumień, [Token|Tokeny]).

% Scanning letters
scanner(Znak, Strumień, [Token|Tokeny]) :-
    char_type(Znak, alpha), !,
    word_reader(Strumień, Znak, Słowo),
    (   keywords(Keywords), member(Słowo, Keywords)
    ->  Token = key(Słowo)
    ;   Token = id(Słowo)
    ),
    get_char(Strumień, NastępnyZnak),
    scanner(NastępnyZnak, Strumień, Tokeny).

% Scanning digits
scanner(Znak, Strumień, [Token|Tokeny]) :-
    char_type(Znak, digit), !,
    number_reader(Strumień, Znak, Liczba),
    atom_number(Liczba, LiczbaInt),
    Token = int(LiczbaInt),
    get_char(Strumień, NastępnyZnak),
    scanner(NastępnyZnak, Strumień, Tokeny).

% Scanning special characters
scanner(Znak, Strumień, [Token|Tokeny]) :-
    separator_reader(Strumień, Znak, Separator),
    Token = sep(Separator),
    get_char(Strumień, NastępnyZnak),
    scanner(NastępnyZnak, Strumień, Tokeny).

word_reader(Strumień, Znak, Słowo) :-
    % Get next character
    peek_char(Strumień, NastępnyZnak),
    % If next character is also a letter
    (   char_type(NastępnyZnak, alpha)
    % Then, read next character and append it
    ->  get_char(Strumień, NastępnyZnak),
        word_reader(Strumień, NastępnyZnak, ResztaSłowa),
        atom_concat(Znak, ResztaSłowa, Słowo)
    % Else return the word
    ;   Słowo = Znak
    ).

number_reader(Strumień, Znak, Liczba) :-
    % Get next character
    peek_char(Strumień, NastępnyZnak),
    % If next character is also a digit
    (   char_type(NastępnyZnak, digit)
    % Then, read next character and append it
    ->  get_char(Strumień, NastępnyZnak),
        number_reader(Strumień, NastępnyZnak, ResztaSłowa),
        atom_concat(Znak, ResztaSłowa, Liczba)
    % Else return the word
    ;   Liczba = Znak
    ).

separator_reader(Strumień, Znak, Separator) :-
    % Get next character
    peek_char(Strumień, NastępnyZnak),
    (   separators(Separators), atom_concat(Znak, NastępnyZnak, MożliwySeparator), member(MożliwySeparator, Separators)
    ->  get_char(Strumień, NastępnyZnak),
        Separator = MożliwySeparator
    ;
        Separator = Znak
    ).

% Filter out sep(end_of_file)
filter_end_of_file([], []).
filter_end_of_file([sep(end_of_file)|T], FilteredTokens) :-
    filter_end_of_file(T, FilteredTokens).
filter_end_of_file([H|T], [H|FilteredTokens]) :-
    H \= sep(end_of_file),
    filter_end_of_file(T, FilteredTokens).

% ---------- PARSER ----------

program([]) --> [].
program([Instrukcja|Program]) --> instrukcja(Instrukcja), [sep(';')], program(Program).

instrukcja(assign(ID, WYRAZENIE)) --> [id(ID)], [sep(:=)], wyrazenie(WYRAZENIE).
instrukcja(read(ID)) --> [key(read)], [id(ID)].
instrukcja(write(WYRAZENIE)) --> [key(write)], wyrazenie(WYRAZENIE).
instrukcja(if(WARUNEK, PROG)) --> [key(if)], warunek(WARUNEK), [key(then)], program(PROG), [key(fi)].
instrukcja(if(WARUNEK, PROG1, PROG2)) --> [key(if)], warunek(WARUNEK), [key(then)], program(PROG1), [key(else)], program(PROG2), [key(fi)].
instrukcja(while(WARUNEK, PROG)) --> [key(while)], warunek(WARUNEK), [key(do)], program(PROG), [key(od)].

wyrazenie(id(ID)) --> [id(ID)].
wyrazenie(int(NUM)) --> [int(NUM)].
wyrazenie(WYRAZ1 + WYRAZ2) --> skladnik(WYRAZ1), [sep(+)], wyrazenie(WYRAZ2).
wyrazenie(WYRAZ1 - WYRAZ2) --> skladnik(WYRAZ1), [sep(-)], wyrazenie(WYRAZ2).
wyrazenie(WYRAZ) --> skladnik(WYRAZ).

skladnik(WYRAZ1 * WYRAZ2) --> czynnik(WYRAZ1), [sep(*)], skladnik(WYRAZ2).
skladnik(WYRAZ1 / WYRAZ2) --> czynnik(WYRAZ1), [sep(/)], skladnik(WYRAZ2).
skladnik(WYRAZ1 mod WYRAZ2) --> czynnik(WYRAZ1), [sep(mod)], skladnik(WYRAZ2).
skladnik(WYRAZ) --> czynnik(WYRAZ).

czynnik(id(ID)) --> [id(ID)].
czynnik(int(NUM)) --> [int(NUM)].
czynnik(WYRAZ) --> [sep('(')], wyrazenie(WYRAZ), [sep(')')].

warunek(WAR1 ; WAR2) --> koniunkcja(WAR1), [key(or)], warunek(WAR2).
warunek(WAR) --> koniunkcja(WAR).

koniunkcja(WAR1 , WAR2) --> prosty(WAR1), [key(and)], koniunkcja(WAR2).
koniunkcja(WAR) --> prosty(WAR).

prosty(WYRAZ1 =:= WYRAZ2) --> wyrazenie(WYRAZ1), [sep(=)], wyrazenie(WYRAZ2).
prosty(WYRAZ1 =\= WYRAZ2) --> wyrazenie(WYRAZ1), [sep(/=)], wyrazenie(WYRAZ2).
prosty(WYRAZ1 < WYRAZ2) --> wyrazenie(WYRAZ1), [sep(<)], wyrazenie(WYRAZ2).
prosty(WYRAZ1 > WYRAZ2) --> wyrazenie(WYRAZ1), [sep(>)], wyrazenie(WYRAZ2).
prosty(WYRAZ1 =< WYRAZ2) --> wyrazenie(WYRAZ1), [sep(=<)], wyrazenie(WYRAZ2).
prosty(WYRAZ1 >= WYRAZ2) --> wyrazenie(WYRAZ1), [sep(>=)], wyrazenie(WYRAZ2).
prosty(WAR) --> [sep('(')], warunek(WAR), [sep(')')].

% ---------- INTERPRETER ----------

% podstaw(+Stare, +ID, +Wartość, -Nowe)
podstaw([], ID, N, [ID = N]).
podstaw([ID=_ | AS], ID, N, [ID=N | AS]) :- !.
podstaw([ID1=W1 | AS1], ID, N, [ID1=W1 | AS2]) :-
    podstaw(AS1, ID, N, AS2).

% pobierz(+Asocjacje, +ID, -Wartość)
pobierz([ID=N | _], ID, N) :- !.
pobierz([_ | AS], ID, N) :-
    pobierz(AS, ID, N).

% wartość(+Wyrażenie, +Asocjacje, -Wartość)
wartość(int(N), _, N).
wartość(id(ID), AS, N) :-
    pobierz(AS, ID, N).
wartość(W1 + W2, AS, N) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N is N1 + N2.
wartość(W1 - W2, AS, N) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N is N1 - N2.
wartość(W1 * W2, AS, N) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N is N1 * N2.
wartość(W1 / W2, AS, N) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N2 =\= 0, N is N1 div N2.
wartość(W1 mod W2, AS, N) :-
    wartość(W1, AS, N1), wartość(W2, AS, N2),
    N2 =\= 0,
    N is N1 mod N2.

% prawda(+Warunek, +Asocjacje)
prawda(W1 =:= W2, AS) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N1 =:= N2.
prawda(W1 =\= W2, AS) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N1 =\= N2.
prawda(W1 < W2, AS) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N1 < N2.
prawda(W1 > W2, AS) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N1 > N2.
prawda(W1 >= W2, AS) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N1 >= N2.
prawda(W1 =< W2, AS) :-
    wartość(W1, AS, N1),
    wartość(W2, AS, N2),
    N1 =< N2.
prawda((W1, W2), AS) :-
    prawda(W1, AS),
    prawda(W2, AS).
prawda((W1; W2), AS) :-
    ( prawda(W1, AS),
    !
    ; prawda(W2, AS)).

% interpreter(+Program, +Asocjacje)
interpreter([], _).
interpreter([read(ID) | PGM], ASSOC) :-
    !,
    read(N),
    integer(N),
    podstaw(ASSOC, ID, N, ASSOC1),
    interpreter(PGM, ASSOC1).
interpreter([write(W) | PGM], ASSOC) :-
    !,
    wartość(W, ASSOC, WART),
    write(WART), nl,
    interpreter(PGM, ASSOC).
interpreter([assign(ID, W) | PGM], ASSOC) :-
    !,
    wartość(W, ASSOC, WAR),
    podstaw(ASSOC, ID, WAR, ASSOC1),
    interpreter(PGM, ASSOC1).
interpreter([if(C, P) | PGM], ASSOC) :- !,
    interpreter([if(C, P, []) | PGM], ASSOC).
interpreter([if(C, P1, P2) | PGM], ASSOC) :-
    !,
    ( prawda(C, ASSOC)
    -> append(P1, PGM, DALEJ)
    ; append(P2, PGM, DALEJ)),
    interpreter(DALEJ, ASSOC).
interpreter([while(C, P) | PGM], ASSOC) :-
    !,
    append(P, [while(C, P)], DALEJ),
    interpreter([if(C, DALEJ) | PGM], ASSOC).
% interpreter(+Program)
interpreter(PROGRAM) :-
    interpreter(PROGRAM, []).


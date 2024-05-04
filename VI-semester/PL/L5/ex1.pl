separators([';', '*', '/', '+', '-', '=', '/=', '<', '>', '>=', '=<', '(', ')', ':=']).
keywords(['read', 'write', 'if', 'then', 'else', 'fi', 'while', 'do', 'od', 'mod', 'or', 'and']).


scanner(Strumień, Tokeny) :-
    get_char(Strumień, Znak),
    scanner(Znak, Strumień, Tokeny).

scanner(end_of_file, _, _) :- !.

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
    Token = int(Liczba),
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

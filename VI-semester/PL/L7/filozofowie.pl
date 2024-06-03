filozofowie :-
    mutex_create(Widelec1),
    mutex_create(Widelec2),
    mutex_create(Widelec3),
    mutex_create(Widelec4),
    mutex_create(Widelec5),

    thread_create(filozof(1, Widelec1, Widelec2), _, []),
    thread_create(filozof(2, Widelec2, Widelec3), _, []),
    thread_create(filozof(3, Widelec3, Widelec4), _, []),
    thread_create(filozof(4, Widelec4, Widelec5), _, []),
    thread_create(filozof(5, Widelec5, Widelec1), _, []).
    
filozof(Numer, LewyWidelec, PrawyWidelec) :-
    repeat,
    % Filozof myśli
    format('[~d] mysli~n', [Numer]),
    sleep(2),
    
    % Filozof stara się podnieść lewy widelec
    % format('[~d] stara się podnieść lewy widelec~n', [Numer]),
    % mutex_lock(LewyWidelec),
    % format('[~d] podniósł lewy widelec~n', [Numer]),

    % Filozof stara się podnieść prawy widelec
    % format('[~d] stara się podnieść prawy widelec~n', [Numer]),
    % mutex_lock(PrawyWidelec),
    % format('[~d] podniósł prawy widelec~n', [Numer]),

    % Filozof je (zajmuje trochę czasu)
    % format('[~d] je~n', [Numer]),
    % sleep(1),

    % Filozof odkłada prawy widelec
    % mutex_unlock(PrawyWidelec),
    % format('[~d] odkłada prawy widelec~n', [Numer]),

    % Filozof odkłada lewy widelec
    % mutex_unlock(LewyWidelec),
    % format('[~d] odkłada lewy widelec~n', [Numer]),
    % fail. % Wraca do początku nieskończonej pętli

     % Filozof stara się podnieść lewy widelec
     (   mutex_trylock(LewyWidelec)
     ->  format('[~d] podniósł lewy widelec~n', [Numer]),
        % Filozof stara się podnieść prawy widelec
        (   mutex_trylock(PrawyWidelec)
        ->  format('[~d] podniósł prawy widelec~n', [Numer]),
            % Filozof je (zajmuje trochę czasu)
            format('[~d] je~n', [Numer]),
            sleep(5),
            % Filozof odkłada prawy widelec
            mutex_unlock(PrawyWidelec),
            format('[~d] odkłada prawy widelec~n', [Numer])
        ;   % Filozof nie może podnieść prawego widelca, więc odkłada lewy
            mutex_unlock(LewyWidelec),
            format('[~d] nie udało się podnieść prawego widelca~n', [Numer]),
            fail
        )
    ;   format('[~d] nie udało się podnieść lewego widelca~n', [Numer]),
    	fail
    ),

    % Filozof odkłada lewy widelec
     mutex_unlock(LewyWidelec),
     format('[~d] odkłada lewy widelec~n', [Numer]),
     fail. % Wraca do początku nieskończonej pętli

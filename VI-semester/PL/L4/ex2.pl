po_lewej(X, Y, [X,Y|_]).
po_lewej(X, Y, [_|Z]) :-
    po_lewej(X, Y, Z).

obok(X,Y,Domy) :-
    po_lewej(X, Y, Domy); po_lewej(Y, X, Domy).

% numer, kolor, narodowość, zwierzę, napój, palenie
rybki(Kto) :-
    Domy = [[1,_,norweg,_,_,_], [2,niebieski,_,_,_,_], [3,_,_,_,mleko,_], [4,_,_,_,_,_], [5,_,_,_,_,_]],
    member([_,czerwony,anglik,_,_,_], Domy),
    po_lewej([_,zielony,_,_,_,_], [_,biały,_,_,_,_], Domy),
    member([_,_,duńczyk,_,herbata,_], Domy),
    obok([_,_,_,_,_,light], [_,_,_,kot,_,_], Domy),
    member([_,żółty,_,_,_,cygaro], Domy),
    member([_,_,niemiec,_,_,fajka], Domy),
    obok([_,_,_,_,_,light], [_,_,_,_,woda,_], Domy),
    member([_,_,_,ptak,_,bez_filtra], Domy),
    member([_,_,szwed,pies,_,_], Domy),
    obok([_,_,_,koń,_,_], [_,żółty,_,_,_,_], Domy),
    member([_,_,_,_,piwo,mentolowe], Domy),
    member([_,zielony,_,_,kawa,_], Domy),
    member([_,_,Kto,rybki,_,_], Domy).

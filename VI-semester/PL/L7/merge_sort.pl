:- consult(merge).

split(IN, OUT1, OUT2) :-
    freeze(IN,
    	   (	IN = [H1, H2 | Rest]
    	   ->	(	OUT1 = [H1 | R1],
    	   		OUT2 = [H2 | R2],
    	   		split(Rest, R1, R2)
    	   	)
    	   ;	(	IN = [H1]
    	   		->	(	OUT1 = [H1],
    	   				OUT2 = []
    	   			)
    	   		;	(	OUT1 = [],
    	   				OUT2 = []
    	   			)
    	   	)
    	   )
    	  ).

merge_sort(IN, OUT) :-
    freeze(IN,
        (   IN = [_, _ | Rest]
        ->  freeze(Rest,
                (   split(IN, Left, Right),
                    merge_sort(Left, SortedLeft),
                    merge_sort(Right, SortedRight),
                    merge(SortedLeft, SortedRight, OUT)
                )
            )
        ;   (   IN = [H1]
            ->  OUT = [H1]
            ;   OUT = []
            )
        )
    ).

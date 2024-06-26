:- module(merge_sort_file, [merge/3, split/3, merge_sort/2]).

merge(IN1, IN2, OUT) :-
    freeze(IN1, 
    	   (	IN1 = [H1 | T1]
    	   ->	freeze(IN2,
    	   		(	IN2 = [H2 | T2]
    	   		->	( 	H1 =< H2
    	   			->	OUT = [H1 | Rest],
    	   				merge(T1, IN2, Rest)
    	   			;	OUT = [H2 | Rest],
    	   				merge(IN1, T2, Rest)
    	   			)
    	   		;	OUT = IN1
    	   		)
    	   	      )
    	   ;	OUT = IN2
    	   )
    	  ). 


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

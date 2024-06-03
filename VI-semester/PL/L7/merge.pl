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


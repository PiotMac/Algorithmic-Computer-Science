all: iterative recursive

test_all: test_iter test_recur

test_iter:
	cd iterative && cargo test && cd ..

test_recur:
	cd recursive && cargo test && cd ..

clean:
	rm -rf iterative/target recursive/target iterative/Cargo.lock recursive/Cargo.lock
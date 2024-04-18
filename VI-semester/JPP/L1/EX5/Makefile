all: Wrapper.gpr iterative recursive
	gprbuild -p -P Wrapper.gpr

iterative:
	cd src && gcc -c -fdump-ada-spec -C ./library_iterative.h -lm && cd ..

recursive:
	cd src && gcc -c -fdump-ada-spec -C ./library_recursive.h -lm && cd ..

clean:
	rm -rf iterative_main recursive_main src/build src/*.ads
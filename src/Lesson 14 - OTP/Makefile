test: build
	erl -pa ebin -config my_crypt -s test run

build:
	erlc -I include -o ebin src/*.erl test/*.erl

clean:
	-rm ebin/*.beam

run:
	erl -pa ebin -config my_crypt -s my_crypt_app start

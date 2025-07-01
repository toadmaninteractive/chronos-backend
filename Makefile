BRANCH = main
DESTDIR = server

all: compile generate

update: update-tree update-deps

clean:
	rm -rf build
	escript rebar clean

update-tree:
	git fetch
#	git reset --hard origin/$(BRANCH)
	git submodule sync
	git submodule update --init --force --recursive

update-deps:
	escript rebar get-deps update-deps

compile:
	escript rebar compile

generate:
	escript rebar generate

install:
	cp -Rf build/* $(DESTDIR)/

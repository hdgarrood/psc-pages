force:

exe: force
		cabal build

bower_components: force
		bower update

all: exe bower_components
		find bower_components/**/src -name '*.purs' | xargs dist/build/psc-pages/psc-pages -o docs


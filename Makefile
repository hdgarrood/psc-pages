force:

bower_components: force
		bower update

all: bower_components
		find bower_components/**/src -name '*.purs' | xargs dist/build/psc-pages/psc-pages -o ~/.purescript/docs/


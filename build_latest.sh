#/bin/sh -x

#set -ex

if [ $# -eq 0 ]
  then
    echo "No arguments supplied"
    exit 1
fi

STRAT=
if [ "$1" = "build" ]; then
	STRAT="$1"
elif [ "$1" = "test" ]; then
	STRAT="$1"
else
	echo "bad argument $1"
	exit 1
fi

rm -fr _opam
#export OPAMSWITCH=4.10.0
eval $(opam env)
opam switch
which ocamlc
which ocaml

echo -e "section_start:`date +%s`:Detecting project to build\r\e[0K run the tests"
LASTDIR=`./detect_latest.sh`

if [ "$LASTDIR" = "." ]; then
	git diff --name-only HEAD HEAD~1
	echo "DO NOTHING"
else
	cd $LASTDIR
  if test -f build.sbt; then
    # Skipping CIing of Scala projects
    exit 0
  fi
  sh ../mylint.sh $LASTDIR

  eval $(opam env)
  if [ "$STRAT" = "build" ]; then
    #[ -d _opam ] || cp -r ~/.opam/4.10 _opam
    #sudo apt install m4 -y
    #opam update
    opam install --deps-only -t -y .
    if [ $? != 0 ]; then
      echo "Installing dependencies failed"
      exit 1
    fi

    echo -e "section_start:`date +%s`:Formatting...\r\e[0KFormatting..."
    dune build @fmt
    if [ $? = 0 ]; then
      echo "Formatting OK"
    else
      echo "Formatting is not decent. Either intergrate ocamlformat to VsCode"
      echo "  or apply it manualy before every commit https://dune.readthedocs.io/en/stable/formatting.html"
      exit 1
    fi

    echo -e "section_start:`date +%s`:Building...\r\e[0KBuilding..."
    dune build
    if [ $? = 0 ]; then
      echo "Running $STRAT in $LASTDIR finished\n"
    else
      exit 1
    fi
  else
    echo -e "section_start:`date +%s`:Installing dependencies\r\e[0KInstalling dependencies..."
    opam install --deps-only -t -y .

    echo -e "section_start:`date +%s`:Testing...\r\e[0K Testing..."
    dune runtest
    if [ $? = 0 ]; then
      echo "Running $STRAT in $LASTDIR finished\n"
    else
      exit 1
    fi
  fi
fi

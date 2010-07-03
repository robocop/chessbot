#!/bin/bash

rm debug/*.ml
cp color.sh debug/
cp config.ml debug/
cp -r cursor/ debug/
sed -e 's/\(^open .*\)/(* \1 *)/' board.ml > debug/board.ml
sed -e 's/\(^open .*\)/(* \1 *)/' chess.ml > debug/chess.ml
sed -e 's/\(^open .*\)/(* \1 *)/' aux.ml > debug/aux.ml
sed -e 's/\(^open .*\)/(* \1 *)/' opens.ml > debug/opens.ml
sed -e 's/\(^open .*\)/(* \1 *)/' get_move.ml > debug/get_move.ml
sed -e 's/\(^open .*\)/(* \1 *)/' stockfish.ml > debug/stockfish.ml
cd debug/
ocaml -init ../.ocamlinit
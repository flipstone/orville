#!/usr/bin/env bash
DIFF_OPTS=(-C 1 --label 'Main.hs (Old)' --label 'Main.hs (New)')
diff "${DIFF_OPTS[@]}" snapshots/Main-1.hs snapshots/Main-2.hs > 1-add-orville-state.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-2.hs snapshots/Main-3.hs > 2-update-runApplication.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-3.hs snapshots/Main-4.hs > 3-add-HasOrvilleState.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-4.hs snapshots/Main-5.hs > 4-add-remaining-typeclasses.patch

#!/usr/bin/env bash
DIFF_OPTS=(-C 1 --label 'Main.hs (Old)' --label 'Main.hs (New)')
diff "${DIFF_OPTS[@]}" snapshots/Main-1.hs snapshots/Main-2.hs > 1-add-readert.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-2.hs snapshots/Main-3.hs > 2-add-orville-typeclasses.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-3.hs snapshots/Main-4.hs > 3-update-runApplication.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-4.hs snapshots/Main-5.hs > 4-update-main.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-5.hs snapshots/Main-6.hs > 5-add-table.patch
diff "${DIFF_OPTS[@]}" snapshots/Main-6.hs snapshots/Main-7.hs > 6-add-migrations.patch

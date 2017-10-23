# Pond

## Objective

Draw a 64 lily pads wide by 128 lily pads high pond with 3,000 turtles and 3,000 frogs distributed randomly across them. No critter should share a pad. Each critter periodically counts surrounding neighbors of the same type. If the same type count is less than 30% of the total number of neighbors (up to eight), the critter will try to move to an open pad in a random direction, some random distance between one and five steps away.

## Elixir / Erlang

### Every Lilypad is a process

Every Lilypad is a process. These are started in two steps by the main process (the 'world'):
1. Fill a map with the {x,y} tuples as keys, and their values will be the PIDs of the Lilypond-process at that location. (optionally passing each of them their position at startup).
2. Connect all Lilypond processes, by telling each of them the PIDs of their horizontal, vertical and diagonal neighbours. When passed these PIDs, a Lilypad will start monitoring them.

When a Lilypad process crashes, this is seen by its neighbours, and it is then removed from their neighbours-list, so no critter can try to travel to a Lilypad that doesn't exist anymore.

The World is linked with trapping exits to the Lilypads, so if one of them crashes, a new one is started that responds to the same location, which is then added to its neighbours (and the neighbours to itself).

### Every Critter is a process.

Every Critter is a process. They are started by the world after the Lilypads have been generated. To do this, the map of (coordinates => pids) is shuffled, and then the first part of that is used as their new homes.

Critters are processes that determine ever so often if they want to move. They use Process.send_after to send themselves a message periodically to see if they are lonely (less than 30% of their eight neighbouring lilypads is taken. I in other words: less than three neighbours)

To check if they are lonely or not, they do the following:
1. Ask the Lilypad they are standing on for a list of its neighbours
2. Ask each of these neighbours if it contains something.

If there isn't enough, we have a list of neighbours that are empty, so we can pick one at random and travel in that direction. After traveling, we repeat this procedure, with the only exception being that the connection we travel in is now fixed (so if we cannot travel further in a certain direction because a Lilypad is already taken, we stop).

'Travel' means that the Critter unlinks and deregisters itself from the Lilypad it came from, and links+registers itself to the Lilypad it went to.

### Problems with this approach

The only 'glaring' problem with this solution is what happens when a Lilypad with a critter on it crashes: Right now, the Critter is not restarted, it just 'drowns'(disappears) when the Lilypad it stood on 'sinks'(crashes). This might be prevented by having the Critter<->Lilypad connection use monitors instead, and have a Critter that had to swim because the Lilypad it stood on does no longer exist periodically ping the World process to ask if there is already a new Lilypad for this location.

To simplify all of the 'check if a Lilypad exists at a certain location' checks, one could use name registration for the processes. However, with the amount of processes we have here, this might actually slow the system down. So this is something we should measure before we go one way or the other.

edit: as suggested below, this answer also has problems with drawing the resulting state.

### Why this approach?

All of the components are easily extendable.
When one Lilypad or Critter dies, the rest of the system is unaffected.
There is no single-process bottleneck. The World process is only used during startup and during 'where is the new lilypad' resolution in the case one of them has crashed and needs to be restarted.


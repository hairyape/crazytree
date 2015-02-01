# crazytree

A Confused Tree's replacement bot in the game The Mana World.

# Running

Gauche Scheme 0.9.4 or higher is required.

    export CRAZYPASS=<password to the crazytree account>
    gosh -I. crazytree.scm

Account name "crazytree" is hard coded in main function in
crazytree.scm. Character name "CrazyTree" is also hard coded in
chat-message function. These needs to be updated if crazytree.scm is
to be run with another account.

# Functionality

The following keywords are recognized when "tree" (or "CrazyTree") is
also mentioned in the same chat message. See function *say-something
in brain.scm for more information.

* "tell me a joke"
* "heal me"
* "what are you"
* "who are you"
* "hi", "hey", "hello", "heya"
* "kick(s)", "shake(s)", "poke(s)", "burns", "waters", "die", "cuts",
  "bites", "shut up"

When "tree" is mentioned but no other keywords are found, a response
may be generated 50% of time. See *no-idea function in brain.scm

When "tree" is not mentioned, the following patterns may be responded
to:

* "hi all", "hello everybody", "hello all", "hello everyone",
  "hi everyone", "hey all" (50% of time)
* "happy * to all" (always)

When tree is told to "shut up", no responses are generated until the
"hi tree" (and similar patterns) are received.

When responding, nick name transform may be applied. See nick-name
function in brain.scm
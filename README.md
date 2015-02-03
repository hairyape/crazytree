# crazytree

A Confused Tree's replacement bot in the game The Mana World.

# Running

Gauche Scheme 0.9.4 or higher is required.

    export CRAZYPASS=<password to the crazytree account>
    gosh -I. crazytree.scm >/dev/null 2>log

Or you can run with supervisor.scm to restart crazytree when it
hangs up or whatever reason

    gosh supervisor.scm gosh -I. crazytree.scm 2>log

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

When the string "talk to me tree" (or "tree talk to me"), Eliza mode
is turned on for the chatter. From now on until the chatter says
"bye" or "shut up", all chat messages from the chatter is considered
part of the conversation with tree. An Eliza script is used instead.

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
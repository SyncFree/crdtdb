# Getting riak-core Up And Running

My aim here is to very quickly, without much in the way of background,
get a cluster of riak-core nodes talking to each other. Most of this is copied from the README for [rebar_riak_core](https://github.com/basho/rebar_riak_core)

Please be aware, this guide is for those who will be developing software for the platform. If you just need to run the platform we'll provide binaries as soon as we have built something.

All the hard work has been done by others. I'll provide a list of
links at the bottom. This is just a real quick start, and I'll follow
up next week when we can turn this cluster into a basic Key/Value
store for storing CvRDTs.

All you need is a computer which is *nix-esque (I'm using OS X, I've
tried these instructions on Fedora 19), and some internets.

OS X Users: If you have used MacPorts, and installed g++, gcc etc that way in the past,
please remove the MacPorts directories from your path.

## Get an Erlang

I strongly recommend you get R16B02 from
<http://www.erlang.org/download.html> It is best to do a source
install (IMO). Grab the src tar, unzip it, have a look at the
`install.md`, and follow the instructions. If your on a Linux, then it
should be as simple as

    ./configure --prefix=$HOME/erlang-R16B02 --disable-hipe --without-odbc


If on OS X then you'll also need

    ./configure --prefix=$HOME/erlang-R16B02 --enable-darwin-64bit --disable-hipe --without-odbc

If `configure` complains about anything other than ODBC or FOP being
missing, then check you have the build requirements (it needs ncurses
dev, ssl dev, g++, things like that.) On OS X running:

    xcode-select --install
    
Solved all my dependancy issues.

After configure, just

    make
    make install

At this point I usually create a link

    ln -s ~/erlang-R16B02 erlang

And add `~/erlang/bin` to my path.

If you now run `erl` you should see something like:-

     15:34:39:db $ erl
     Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

    Eshell V5.10.3  (abort with ^G)
    1>

And you have an Erlang! Hit `Ctrl-g` then type `q` to quit the Erlang shell.

## Clone rebar_riak_core from github

You need `git`, if you don't have it, please get it as we'll be using it
throughout the project (as well as [github](github.com))

    git clone https://github.com/basho/rebar_riak_core.git

Rebar is an erlang build tool. Rebar Riak Core are some templates that
rebar can use to create the skeleton of a riak-core application.

    cd rebar_riak_core
    git checkout rdb/syncfree
    make

I had to make a branch that uses the latest version of riak-core. Our own SyncFree branch (let's hug later!)

You should see something like:

    mkdir -p /Users/russell/.rebar/templates
    cp riak* /Users/russell/.rebar/templates

But with your home, not mine. Now you have the rebar templates you need.

## Make the App Skeleton

    cd ../
    mkdir crdtdb
    cd crdtdb

You can call that new directory whatever you like, but I figured we
should make a simple Key->CRDT database. Now you need `rebar` the
build tool itself. You can clone it from github and build it, or you
can download a pre-built binary.

    wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar
    
If you don't have `wget` you can use `curl -o`, or just grab the file with your web browser and save it
in `crdtdb` and then `chmod u+x rebar`

Now let's create the application:

    ./rebar create template=riak_core appid=crdtdb

That should be it.

## Build the "App" for the first time

Riak-core should probably go on a bit of diet. There are a "few"
dependencies it needs. You might want to put the kettle on and make a
cup of tea after typing the next command.

    make rel

Rebar will now pull all the dependencies it needs from github, and
build the application, and make an erlang "release" of a single node
for us. You can watch the words go by, or do something else. I'm going
for coffee, back in a bit...

## Ping?

If all went well (email me if it didn't, or put it on the SyncFree
tech mailing list, for maximum help), then you should be able to start
a node of `crdtdb`.

    15:55:05:crdtdb $ rel/crdtdb/bin/crdtdb console
    (elided)
    Eshell V5.10.3  (abort with ^G)
    (crdtdb@127.0.0.1)1> crdtdb:ping().
    {pong,1118962191081472546749696200048404186924073353216}
    (crdtdb@127.0.0.1)3>

What you should see is a `pong` response, followed by a big
number. The number is the partition that responded to the `ping`
request. Try it a few more times, different partitions will respond.

Again `Ctrl-g` and `q` to quit the shell.


## Make a cluster

What we really want is a distributed system. So let's make one.

    make devrel

Will generate 4 nodes of `crdtdb` on your local machine, in
`./dev`. When that is done, we should start them all up.

    for d in dev/dev*; do $d/bin/crdtdb start; done

And check that they're working:

    for d in dev/dev*; do $d/bin/crdtdb ping; done
    pong
    pong
    pong
    pong


At this point you have 4 single node applications running. We need to
join them together in a cluster:

    for d in dev/dev{2,3,4}; do $d/bin/crdtdb-admin cluster join 'crdtdb1@127.0.0.1'; done
    Success: staged join request for 'crdtdb2@127.0.0.1' to 'crdtdb1@127.0.0.1'
    Success: staged join request for 'crdtdb3@127.0.0.1' to 'crdtdb1@127.0.0.1'
    Success: staged join request for 'crdtdb4@127.0.0.1' to 'crdtdb1@127.0.0.1'

Sends the requests to node1, which we can now tell to build the cluster:

     dev/dev1/bin/crdtdb-admin cluster plan
     ...
     dev/dev1/bin/crdtdb-admin cluster commit

Have a look at the `member-status` to see that the cluster is balancing.

    dev/dev1/bin/crdtdb-admin member-status
    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid     100.0%     25.0%    'crdtdb1@127.0.0.1'
    valid       0.0%     25.0%    'crdtdb2@127.0.0.1'
    valid       0.0%     25.0%    'crdtdb3@127.0.0.1'
    valid       0.0%     25.0%    'crdtdb4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0


Wait a while, and look again, and you should see a fully balanced
cluster.

    dev/dev1/bin/crdtdb-admin member-status
    ================================= Membership ==================================
    Status     Ring    Pending    Node
    -------------------------------------------------------------------------------
    valid      25.0%      --      'crdtdb1@127.0.0.1'
    valid      25.0%      --      'crdtdb2@127.0.0.1'
    valid      25.0%      --      'crdtdb3@127.0.0.1'
    valid      25.0%      --      'crdtdb4@127.0.0.1'
    -------------------------------------------------------------------------------
    Valid:4 / Leaving:0 / Exiting:0 / Joining:0 / Down:0


## Remote calls

We don't have a client, or an API, but we can still call into the
cluster using distributed erlang.

Let's start a node:

    erl -name 'client@127.0.0.1' -setcookie crdtdb

First check that we can connect to the cluster:

     (cli@127.0.0.1)1> net_adm:ping('crdtdb3@127.0.0.1').
     pong

Then we can rpc onto any of the nodes and call `ping`:

    (cli@127.0.0.1)2> rpc:call('crdtdb1@127.0.0.1', crdtdb, ping, []).
    {pong,662242929415565384811044689824565743281594433536}
    (cli@127.0.0.1)3>

And you can shut down your cluster like

    for d in dev/dev*; do $d/bin/crdtdb stop; done
    
When you start it up again, it will still be a cluster.

## That's it

Now you have a running riak-core application cluster (I'll cover WHAT it is we have actually done in the next tutorial.) And next time we'll add some code to turn it into a simple distributed database.



If you're eager to move on, then there are some good links
[here](http://basho.com/where-to-start-with-riak-core/) it is old, but
mostly applies. I find
[try-try-try](https://github.com/rzezeski/try-try-try) the most
valuable resource.

Good luck, mail the SyncFree tech list with problems, ideas,
suggestions, etc.

Don't forget about the Erlang and Riak resources on the [wiki](https://syncfree.lip6.fr/dokuwiki/doku.php?id=syncfree:workpackages:wp2)


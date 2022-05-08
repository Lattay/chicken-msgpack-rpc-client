### About

msgpack-rpc-client is a [Chicken 5](call-cc.org) egg that implement a client for
the [MsgPack-RPC](https://github.com/msgpack-rpc/msgpack-rpc) protocol.

It aims at providing a Lispy interface to this protocol with support of
asyncronous requests and possibility to respond to server-to-client requests.

## Features

- [x] Syncronous requests
- [x] Async callback based requests
- [x] Async promise based requests
- [x] Async multi-threaded requests
- [x] Async single-threaded requests
- [x] notifications
- [x] thread-safe operations
- [x] server-to-client requests and notifications
- [x] transport over TCP/IPv4
- [x] transport over Unix socket
- [ ] transport over other channels through an extension interface
- [ ] testing
    - [x] transport
    - [x] client-to-server
    - [ ] server-to-client

## Building and testing

msgpack-rpc-client depends on the following eggs:
- [srfi-1](http://wiki.call-cc.org/eggref/5/srfi-1)
- [srfi-69](http://wiki.call-cc.org/eggref/5/srfi-69)
- [socket](http://wiki.call-cc.org/eggref/5/socket)
- [msgpack](http://wiki.call-cc.org/eggref/5/msgpack)

For development, building and testing is done through a `make`. `make` build
everything. `make test` run tests.

Beware that testing is done against the
[aio-msgpack-rpc](https://pypi.org/project/aio-msgpack-rpc) python module.
Testing also require  You must install it before running the test suite.

## Other notice

Server-to-client requests and notifications is a very interesting feature that
is not part of the standard protocol, and thus not present in many server
implementations. However, practical implementations like the
[Neovim](https://neovim.io/doc/user/api.html) one uses it to great effect to
establish a event subscription system.

While the ability to receive and react to such messages is implemented in this
package, it is not tested as the Python server I use for testing does not
support the feature. While I plan on implementing the feature in the Python
server and a future Chicken server, this feature is currently provided as
experimental.

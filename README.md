# Thesis Window Manager

This window manager is a fork of StumpWM  designed to include features from
Oberon A2 (ZUI with big desktop), acme (tag bar with editable commands), and
some basic features from popular floating window managers. It is currently
in a pre-alpha state, and is only recommended for basic testing.

## Setup
The setup steps are very similar to those for StumpWM, plus a few extra
dependencies.

First, you'll need to install the `dbus` quicklisp package to communicate with
the compton fork required for the ZUI functionality.

```
(ql:quickload "dbus")
```

Next, you'll need to install and run the compton fork, which needs to be run
at startup.

Finally, there are some modifications in the .stumpwmrc included in this
repository that may be helpful in running the WM.

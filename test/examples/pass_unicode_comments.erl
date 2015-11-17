%%% @doc FSM to syncronize differences for the remote DC replication
%%%                             ┌────────┐
%%%                             │   not  │
%%%                             ▼  empty │
%%%  ┌───────────┐        ┌───────────┐  │
%%%  │   comp    │───────▶│ sync_diff │──┘
%%%  └───────────┘        └───────────┘
%%%                      empty  │
%%%                             │
%%%         ┌───────────────────┘
%%%         │                   ┌────────┐
%%%         │                   │    not │
%%%         ▼       empty       ▼   empty│
%%%   ┌───────────┐       ┌───────────┐  │
%%%┌─▶│    get    │──────▶│   push    │──┘
%%%│  └───────────┘       └───────────┘
%%%│  not   │                   │
%%%│ empty  │                   │  empty
%%%└────────┘                   │
%%%                             │
%%%                             ▼
%%%                       ┌───────────┐
%%%                       │   done    │
%%%                       └───────────┘
%%% @end
-module(pass_unicode_comments).

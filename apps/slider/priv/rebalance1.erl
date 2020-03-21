%%      A         B
%%     / \       / \
%%    B   RA => LB  A
%%   / \           / \
%%  LB RB         RB  RA
rebalance({node, KA, PA, VA, {node, KB, PB, VB, LB, RB}, RA}) when PA > PB ->
    {node, KB, PB, VB, LB, rebalance({node, KA, PA, VA, RB, RA})};
%%      A            B
%%     / \          / \
%%    LA  B   =>   A   RB
%%       / \      / \
%%      LB RB    LA  LB
rebalance({node, KA, PA, VA, LA, {node, KB, PB, VB, LB, RB}}) when PA > PB ->
    {node, KB, PB, VB, rebalance({node, KA, PA, VA, LA, LB}), RB};
%% All Good.
rebalance(Node) ->
    Node.

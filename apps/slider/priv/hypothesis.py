from hypothesis.strategies import integers, lists
from hypothesis import given

@given(lists(integers()))
def test_pop_in_sorted_order(ls):
    h = heapnew()
    for l in ls:
        heappush(h, l)
    r = []
    while not heapempty(h):
        r.append(heappop(h))
    assert r == sorted(ls)

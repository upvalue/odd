import odd, pytest

# Fixtures don't seem to work automatically?
state = odd.State()

def test_booleans():
    assert state.parse("#t") == [odd.TRUE]
    assert state.parse("#f") == [odd.FALSE]
    assert state.parse("#t #f #t #f") == [odd.TRUE, odd.FALSE, odd.TRUE, odd.FALSE]

def test_symbols():
    assert state.parse("hello") == [state.make_symbol("hello")]
    assert state.parse("hello-how-are-you!") == [state.make_symbol("hello-how-are-you!")]
    assert state.parse("/hello") == [state.make_symbol("/hello")]

def test_comments():
    assert state.parse("// Hello world") == []

if __name__ == "__main__":
    pytest.main()

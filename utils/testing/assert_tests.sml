use "testing.sml";

open SmlTests;

exception FooExn;
exception BarExn;

test("assert_equals with equal output PASSED",
     assert_equals(TEST_PASSED,
		   assert_equals("foo", "foo", string_formatter),
		   string_formatter));

test("assert_equals is not equal output error",
     assert_equals("Expected 'foo' but got 'bar'",
		   assert_equals("foo", "bar", string_formatter),
		   string_formatter));

test("assert_equals_any passes if arguments are equal",
     assert_equals(TEST_PASSED, 
		   assert_equals_any((42,"hi"), (42,"hi")), 
		   string_formatter));

test("assert_equals_any fails if arguments are different",
     assert_equals(TEST_FAILED, 
		   assert_equals_any("foo", "bar"), 
		   string_formatter));

test("assert_raises with correct exception passes",
     assert_equals(TEST_PASSED,
		   assert_raises(fn () => raise FooExn, (), FooExn),
		   string_formatter));

test("assert_raises without exception raised gives error",
     assert_equals("No FooExn was raised",
		   assert_raises(fn () => (), (), FooExn),
		   string_formatter));

test("assert_raises with wrong exception gives error",
     assert_equals("Wrong exception raised: BarExn",
		   assert_raises(fn () => raise BarExn, (), FooExn),
		   string_formatter));

test("assert_raises calls fn with provided arguments",
     assert_equals(TEST_PASSED,
		   assert_raises(fn (a,b,c) => if a=1 andalso b="foo" andalso c
					       then raise FooExn
					       else "ok",
				 (1, "foo", true),
				 FooExn),
		   string_formatter));

run();

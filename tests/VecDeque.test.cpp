// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/VecDeque.h"

#include "doctest.h"
#include <memory>

TEST_SUITE_BEGIN("VecDequeTests");

TEST_CASE("forward_queue_test_no_initial_capacity")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<int> queue{};

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(i);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 11);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), j);
        CHECK_EQ(queue.back(), 9);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("forward_queue_test")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<int> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(i);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 13);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), j);
        CHECK_EQ(queue.back(), 9);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("forward_queue_test_initializer_list")
{
    Luau::VecDeque<int> queue{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), j);
        CHECK_EQ(queue.back(), 9);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("reverse_queue_test")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<int> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_front(i);
    // q: 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 13);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), 9);
        CHECK_EQ(queue.back(), j);

        REQUIRE(!queue.empty());
        queue.pop_back();
    }
}

TEST_CASE("random_access_queue_test")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<int> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(i);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.at(j), j);
        CHECK_EQ(queue[j], j);
    }
}

TEST_CASE("clear_works_on_queue")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<int> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(i);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], j);

    queue.clear();
    CHECK(queue.empty());
    CHECK(queue.size() == 0);
}

TEST_CASE("pop_front_at_end")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<int> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    // setting up the internal buffer to be: 1234567890 by the end (i.e. 0 at the end of the buffer)
    queue.push_front(0);

    for (int i = 1; i < 10; i++)
        queue.push_back(i);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), j);
        CHECK_EQ(queue.back(), 9);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("pop_back_at_front")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<int> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    // setting up the internal buffer to be: 9012345678 by the end (i.e. 9 at the front of the buffer)
    queue.push_back(0);

    for (int i = 1; i < 10; i++)
        queue.push_front(i);
    // q: 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), 9);
        CHECK_EQ(queue.back(), j);

        REQUIRE(!queue.empty());
        queue.pop_back();
    }
}

TEST_CASE("queue_is_contiguous")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<int> queue{};

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(i);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 11);
    CHECK(queue.is_contiguous());
}

TEST_CASE("queue_is_not_contiguous")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<int> queue{};

    REQUIRE(queue.empty());

    for (int i = 5; i < 10; i++)
        queue.push_back(i);
    for (int i = 4; i >= 0; i--)
        queue.push_front(i);
    // buffer: 56789......01234
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 11);
    CHECK(!queue.is_contiguous());

    // checking that it is indeed sequential integers from 0 to 9
    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], j);
}

TEST_CASE("shrink_to_fit_works")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<int> queue{};

    REQUIRE(queue.empty());

    for (int i = 5; i < 10; i++)
        queue.push_back(i);
    for (int i = 4; i >= 0; i--)
        queue.push_front(i);
    // buffer: 56789......01234
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    REQUIRE_EQ(queue.capacity(), 11);
    CHECK(!queue.is_contiguous());

    // checking that it is indeed sequential integers from 0 to 9
    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], j);

    queue.shrink_to_fit();
    // shrink to fit always makes a contiguous buffer
    CHECK(queue.is_contiguous());
    // the capacity should be exactly the size now
    CHECK_EQ(queue.capacity(), queue.size());

    REQUIRE(!queue.empty());

    // checking that it is still sequential integers from 0 to 9
    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], j);
}

// To avoid hitting SSO issues, we need sufficiently long strings.
// This list of test strings consists of quotes from Ursula K. Le Guin.
const static std::string testStrings[] = {
    "Love doesn't just sit there, like a stone, it has to be made, like bread; remade all the time, made new.",
    "People who deny the existence of dragons are often eaten by dragons. From within.",
    "It is good to have an end to journey toward; but it is the journey that matters, in the end.",
    "We're each of us alone, to be sure. What can you do but hold your hand out in the dark?",
    "When you light a candle, you also cast a shadow.",
    "You cannot buy the revolution. You cannot make the revolution. You can only be the revolution. It is in your spirit, or it is nowhere.",
    "To learn which questions are unanswerable, and not to answer them: this skill is most needful in times of stress and darkness.",
    "What sane person could live in this world and not be crazy?",
    "The only thing that makes life possible is permanent, intolerable uncertainty: not knowing what comes next.",
    "My imagination makes me human and makes me a fool; it gives me all the world and exiles me from it."
};

TEST_CASE("string_queue_test_no_initial_capacity")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<std::string> queue;

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(testStrings[i]);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 11);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), testStrings[j]);
        CHECK_EQ(queue.back(), testStrings[9]);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("string_queue_test")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<std::string> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(testStrings[i]);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 13);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), testStrings[j]);
        CHECK_EQ(queue.back(), testStrings[9]);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("string_queue_test_initializer_list")
{
    Luau::VecDeque<std::string> queue{
        testStrings[0],
        testStrings[1],
        testStrings[2],
        testStrings[3],
        testStrings[4],
        testStrings[5],
        testStrings[6],
        testStrings[7],
        testStrings[8],
        testStrings[9],
    };
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), testStrings[j]);
        CHECK_EQ(queue.back(), testStrings[9]);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("reverse_string_queue_test")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<std::string> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_front(testStrings[i]);
    // q: 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 13);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), testStrings[9]);
        CHECK_EQ(queue.back(), testStrings[j]);

        REQUIRE(!queue.empty());
        queue.pop_back();
    }
}

TEST_CASE("random_access_string_queue_test")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<std::string> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(testStrings[i]);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.at(j), testStrings[j]);
        CHECK_EQ(queue[j], testStrings[j]);
    }
}

TEST_CASE("clear_works_on_string_queue")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<std::string> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(testStrings[i]);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], testStrings[j]);

    queue.clear();
    CHECK(queue.empty());
    CHECK(queue.size() == 0);
}

TEST_CASE("pop_front_string_at_end")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<std::string> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    // setting up the internal buffer to be: 1234567890 by the end (i.e. 0 at the end of the buffer)
    queue.push_front(testStrings[0]);

    for (int i = 1; i < 10; i++)
        queue.push_back(testStrings[i]);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), testStrings[j]);
        CHECK_EQ(queue.back(), testStrings[9]);

        REQUIRE(!queue.empty());
        queue.pop_front();
    }
}

TEST_CASE("pop_back_string_at_front")
{
    // initial capacity set to 5 so that a grow is necessary
    Luau::VecDeque<std::string> queue;
    queue.reserve(5);

    REQUIRE(queue.empty());

    // setting up the internal buffer to be: 9012345678 by the end (i.e. 9 at the front of the buffer)
    queue.push_back(testStrings[0]);

    for (int i = 1; i < 10; i++)
        queue.push_front(testStrings[i]);
    // q: 9, 8, 7, 6, 5, 4, 3, 2, 1, 0

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    for (int j = 0; j < 10; j++)
    {
        CHECK_EQ(queue.front(), testStrings[9]);
        CHECK_EQ(queue.back(), testStrings[j]);

        REQUIRE(!queue.empty());
        queue.pop_back();
    }
}

TEST_CASE("string_queue_is_contiguous")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<std::string> queue{};

    REQUIRE(queue.empty());

    for (int i = 0; i < 10; i++)
        queue.push_back(testStrings[i]);
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 11);
    CHECK(queue.is_contiguous());
}

TEST_CASE("string_queue_is_not_contiguous")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<std::string> queue{};

    REQUIRE(queue.empty());

    for (int i = 5; i < 10; i++)
        queue.push_back(testStrings[i]);
    for (int i = 4; i >= 0; i--)
        queue.push_front(testStrings[i]);
    // buffer: 56789......01234
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    CHECK_EQ(queue.capacity(), 11);
    CHECK(!queue.is_contiguous());

    // checking that it is indeed sequential integers from 0 to 9
    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], testStrings[j]);
}

TEST_CASE("shrink_to_fit_works_with_strings")
{
    // initial capacity is not set, so this should grow to be 11
    Luau::VecDeque<std::string> queue{};

    REQUIRE(queue.empty());

    for (int i = 5; i < 10; i++)
        queue.push_back(testStrings[i]);
    for (int i = 4; i >= 0; i--)
        queue.push_front(testStrings[i]);
    // buffer: 56789......01234
    // q: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    REQUIRE(!queue.empty());
    REQUIRE(queue.size() == 10);

    REQUIRE_EQ(queue.capacity(), 11);
    CHECK(!queue.is_contiguous());

    // checking that it is indeed sequential integers from 0 to 9
    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], testStrings[j]);

    queue.shrink_to_fit();
    // shrink to fit always makes a contiguous buffer
    CHECK(queue.is_contiguous());
    // the capacity should be exactly the size now
    CHECK_EQ(queue.capacity(), queue.size());

    REQUIRE(!queue.empty());

    // checking that it is still sequential integers from 0 to 9
    for (int j = 0; j < 10; j++)
        CHECK_EQ(queue[j], testStrings[j]);
}

struct TestStruct
{
};

// Verify that elements pushed to the front of the queue are properly destroyed when the queue is destroyed.
TEST_CASE("push_front_elements_are_destroyed_correctly")
{
    std::shared_ptr<TestStruct> t = std::make_shared<TestStruct>();
    {
        Luau::VecDeque<std::shared_ptr<TestStruct>> queue{};
        REQUIRE(queue.empty());
        queue.reserve(10);
        queue.push_front(t);
        queue.push_front(t);
        REQUIRE(t.use_count() == 3); // Num of references to the TestStruct instance is now 3
        // <-- call destructor here
    }

    // At this point the destructor should be called and we should be back down to one instance of TestStruct
    REQUIRE(t.use_count() == 1);
}

TEST_SUITE_END();

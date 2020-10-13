using System;
using System.Collections.Generic;
using Microsoft.FSharp.Core;
using Rogz.Context.Base;
using Rogz.Context.Linq;


namespace CsTesting
{
    class CsProgram
    {
        static void EitherTests()
        {
            var query =
                from a in Either<string, int>.NewRight(1)
                from b in Either<string, int>.NewRight(2)
                join c in Either<string, int>.NewRight(3) on 1 equals 1
                join d in Either<string, int>.NewRight(4) on 1 equals 1
                select a + b + c + d;

            var bifunctor1 = Either<int, string>.NewLeft(1).Select(x => $"passed: {x}", _ => "failed");
            var bifunctor2 = Either<int, string>.NewRight("1").Select(_ => "failed", x => $"passed: {x}");

            Console.WriteLine($"Either Query: {query}");
            Console.WriteLine($"Bifunctor via _.Select overload: {bifunctor1}  &  {bifunctor2}");
        }


        static void MaybeTests()
        {
            var query =
                from a in Maybe<int>.NewJust(1)
                from b in Maybe<int>.NewJust(2)
                join c in Maybe<int>.NewJust(3) on 1 equals 1
                join d in Maybe<int>.NewJust(4) on 4 equals d into grp
                from d in grp
                select a + b + c + d;

            var joinby = Maybe.Unit(1).GroupJoin(Maybe.Unit(2), (x, y) => y > x, (x, y) => new { X = x, Y = y, Sum = x + y });

            var appendInts = Maybe.append(Maybe.Unit(1), Maybe.Unit(2));
            var appendStrings = Maybe.append(first: Maybe.Unit("1"), second: Maybe.Unit("2"));
            var summedStrings = Maybe.sum<List<Maybe<string>>, string>(new List<Maybe<string>>() { Maybe.Unit("1"), Maybe.Unit("2"), Maybe.Unit("3"), });

            Console.WriteLine($"Maybe Query: {query}");
            Console.WriteLine($"Maybe join via GroupJoin (non-query version): {joinby}");
            Console.WriteLine($"Maybe append ints via (+): {appendInts}");
            Console.WriteLine($"Maybe append strings via (+): {appendStrings}");
            Console.WriteLine($"Maybe summed strings via `sum`: {summedStrings}");
        }



        [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE0060:Remove unused parameter", Justification = "<Pending>")]
        static void Main(string[] args)
        {
            //EitherTests();
            MaybeTests();
        }
    }
}

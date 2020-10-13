In terms of monadic sequencing, in Base:
	have unit, bind, and(?) flatten
	Workflow module with non-sealed builder type with:
		Return, ReturnFrom, Bind, and? Zero (note that Zero may depend on other factors)
	*thats it for Monad. so maybe (? BIG maybe ?) remove [foldlM, foldrM, fixM, guard, filter, join, et al]
	*> no - keep filter for sure
	& perhaps move the more esoteric stuff into something like Haskell's "Control.<X>" package

In Context.Workflow (.Query may be a better name?):
	the point is to make programming against the query system easier on read, rather than for performance
	meanwhile, the default/simple workflow in Base in suitable for the vast bulk of needs
> In this Query module, we can extend the one from Base and add such things as the following:
	Where[where] = filter
	GroupJoin[join] = join
	Select(select) = map
	Zip(zip) = map2 (although, it's technically zipWith...)

	& since this is meant to be consumed by F# libs, it can include more functional stuff like:
		guard et all
		NOTE: it's almost like a 1 for 1 pairing with the .NET lib, so all the Linq methods and overloads can be included
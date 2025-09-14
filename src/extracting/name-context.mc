-- name-context.mc
--
-- This file defines a key part of the Miking Doc Gen project: the NameContext.
-- We want to be able to access the URL of any object documentation page
-- from anywhere in the code.
--
-- Let's assume this code:
--
-- type X = Int
-- let z: X = 2
--
-- Imagine we want to guess the URL of X from z's point of view.
-- We need to take the closest X URL. For this, we can use a HashMap
-- binding names to their URLs. Since we have multiple bindings,
-- we need a single HashMap shared by all objects.
-- In this context, the map will have two entries:
-- X -> /type-x.mc
-- z -> /let-z.mc
-- So when z wants to get X’s URL, no problem.
--
-- Let’s take another example:
-- type X = Int
-- let y: X = 2
-- let X = String
-- let z: X = "hey"
-- Here, we have an issue: if y and z both try to fetch X’s URL,
-- one of them will get the wrong link to X, and the system breaks.
-- One solution is to add an ID to each object, and make the HashMap
-- bind names to arrays.
-- So now, the map looks like this:
-- X -> [type-x_0.mc (0), type-x_1.mc (2)]
-- y -> [let-y.mc (1)]
-- z -> [let-z.mc (3)]
-- Now, when z requests X, it also provides its ID (3).
-- We then select the X with the greatest ID lower than 3.
--
-- Now comes the last problem. Imagine this code:
-- type X = Int
-- let y: X = type X = Int in 2
-- let z: X = 1
-- Here, our HashMap doesn’t work anymore. The nested X will have ID 2,
-- so when z fetches X with ID 3, it will get the nested link instead.
-- To prevent this, we also provide the namespace of z, which is
-- file.mc/let-z if the file is named file.mc.
-- Then, we extract the part of the namespace after the file, here "let-z".
-- If we repeat the process:
-- X_0 -> type-X
-- X_2 -> let-y/type-X
-- We then return the link such that:
--   - the no-file part is included in the caller’s no-file part, and  
--   - it has the greatest ID still lower than the caller’s ID.
-- This ensures we only get links in our scope.  
-- Since, at the end of the day, all objects are placed sequentially
-- regardless of their origin file, this works regardless of the include structure.

include "../global/util.mc"

type NameSpaceEntry a = { entry: a, id: Int, namespace: String }

type NameContext = all a. {
    map: HashMap String [NameSpaceEntry a]
}

let nameContextInsert : all a. NameContext -> String -> NameSpaceEntry a = lam ctx. lam name. lam entry.
    let entries = optionMap (cons entry) (hmLookup name ctx.map) in
    { ctx with map = hmInsert name entries ctx.map }

let nameContextFetch : NameContext -> String -> Int -> String -> Option String =
    lam ctx. lam name. lam id. lam namespace.
    optionMap (
        lam entries.
        optionMap (lam entry. entry.entry)
        (find (
            lam entry.
            and (lti entry.id id) (strStartsWith entry.namespace namespace)
        ) entries)
    ) hmLookup name ctx.map

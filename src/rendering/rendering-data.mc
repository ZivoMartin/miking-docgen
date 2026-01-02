include "../extracting/objects.mc"

type RenderingData = use Objects in {
    obj: Object,

    left : String,
    right : String,

    tests: String
}

let renderingDataRaw : RenderingData -> String =
    lam data.
    concat data.left data.right

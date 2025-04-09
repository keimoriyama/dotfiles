{ sources, epkgs }:
{
    reftex = epkgs.melpaBuild {
    pname = "reftex";
    src = sources.reftex.src;
}
}
{nixpack}:
with nixpack.mkSources;
{
layers = [
{
  streamly-examples = local ./.;
}
];
}

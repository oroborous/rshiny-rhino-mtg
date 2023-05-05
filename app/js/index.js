// $ cd ../.rhino
// $ npm install
// > rhino::build_js()

export function formatDollars(value) {
  if (value > 1_000_000) {
    return `$${(value / 100_000).toFixed(1)}M`
  }
  if (value > 1000) {
    return `$${(value / 100).toFixed(1)}K`
  }
  return `$${value}`;
}

export function cardsBySetTooltip(value) {
  return value;
}

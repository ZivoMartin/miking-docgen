let searchHtml: String = 
"<div id=\"search-container\">
  <input id=\"search-bar\" type=\"text\" placeholder=\"Type to search\" />
  <div id=\"search-results\"></div>
</div>"

let searchCss: String =
"
/* Search Engine */
#search-container {
    padding: 15px;
    display: flex;
    justify-content: center;
    position: relative;
}

#search-bar {
    width: 60%;
    max-width: 600px;
    padding: 10px 14px;
    font-size: 15px;
    border: none;
    border-radius: 3px;
    background: var(--searchBarBGColor);
    color: var(--searchBarTextColor);
    box-shadow: inset 0 1px 2px var(--searchBarShadowColor);
}

#search-bar::placeholder {
    color: var(--searchBarPlaceholderColor);
}

#search-results {
    position: absolute;
    top: 100%;
    left: 50%;
    transform: translateX(-50%);
    width: 60%;
    max-width: 600px;
    display: flex;
    flex-direction: column;
    gap: 6px;
    z-index: 1000;
    border-radius: 6px;
    padding: 6px 0;
    box-shadow: 0 4px 12px var(--searchResultsShadowColor);
}

#search-results a:first-child {
    margin-top: 0;
}

#search-bar:focus {
    outline: none;
}

#search-results a {
    display: flex;
    align-items: center;
    padding: 10px 14px;
    border-radius: 6px;
    color: var(--searchResultItemTextColor);
    font-size: 15px;
    margin: 0;
    transition: background 0.2s, transform 0.15s;
    backdrop-filter: blur(6px);
}

#search-results a + a {
    border-top: 1px solid var(--searchResultItemBorderColor);
}

#search-results a:hover {
    background: var(--searchResultItemHoverBGColor);
    transform: translateX(4px);
    cursor: pointer;
}

#search-results a:active {
    background: var(--searchResultItemActiveBGColor);
    transform: translateX(2px);
}

#search-results:empty {
    display: none;
}

.highlight {
    font-weight: bold;
    color: var(--searchHighlightColor);
}
"

type SearchDictObj = { name: String, link: String }

let searchJsPath = "search.js"

let searchJs: [SearchDictObj] -> String = lam objects.
let dict = strJoin "," (map (lam obj. join ["\n  { \"name\": \"", obj.name, "\", \"link\": \"", obj.link, "\" }"]) objects) in
join [
"const results = [", dict, "];

const searchBar = document.getElementById(\"search-bar\");
const resultsDiv = document.getElementById(\"search-results\");

let inputProcess = () => {
    const query = searchBar.value.trim();
    resultsDiv.innerHTML = \"\";

    if (query.length === 0) return;

    const candidates = results.filter(word =>
        word[\"name\"].includes(query)
    );

    const sorted = candidates.sort((a, b) => a[\"name\"].length - b[\"name\"].length);

    const top = candidates
      .sort((a, b) => a[\"name\"].length - b[\"name\"].length)
      .slice(0, 10);


    for (const candidate of top) {
        const choice = document.createElement(\"a\");
        const regex = new RegExp(`(${query})`, \"gi\");
        const highlighted = candidate[\"name\"].replace(regex, \"<span class='highlight'>$1</span>\");
        choice.innerHTML = highlighted;
        choice.href = candidate[\"link\"];
        resultsDiv.appendChild(choice);
    }
};

searchBar.addEventListener(\"input\", inputProcess);
searchBar.addEventListener(\"focus\", inputProcess);
searchBar.addEventListener(\"blur\", () => {
  setTimeout(() => {
    resultsDiv.innerHTML = \"\";
  }, 150); // small delay so the <a> click works
});
"]

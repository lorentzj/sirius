export function getElementIndex(element) {
    let index = 0;
    while (element.previousElementSibling !== null) {
        element = element.previousElementSibling;
        index++;
    }
    return index;
}
export function debounce(callback, bounceMs = 50) {
    let timeoutId = null;
    return (e) => {
        if (timeoutId !== null) {
            clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(() => {
            callback(e);
        }, bounceMs);
    };
}

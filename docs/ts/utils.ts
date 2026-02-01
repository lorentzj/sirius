export function getElementIndex(element: Element): number {
    let index = 0;
    while (element.previousElementSibling !== null) {
        element = element.previousElementSibling;
        index++;
    }
    return index;
}

export function debounce<T>(callback: (e: T) => void, bounceMs: number = 50) {
    let timeoutId: number | null = null;
    return (e: T) => {
        if(timeoutId !== null) {
            clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(() => {
            callback(e);
        }, bounceMs);
    };
}
;
export function constraint_name(constraint) {
    if (constraint.data.Eq !== undefined) {
        return `${constraint.data.Eq[0]} == ${constraint.data.Eq[1]}`;
    }
    else {
        return '!!!!';
    }
}

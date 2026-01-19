function returnN = wait_for_n_or_x()
%WAIT_FOR_N_OR_X holds until n or x is pressed. Returns 1 for n or 0 for x
nKey = KbName('N');
xKey = KbName('X');
while true
    [~,~, keyCode] = KbCheck;
    if (keyCode(xKey))
        returnN = 0;
        break;
    elseif keyCode(nKey)
        returnN = 1;
        break;
    end
end
end


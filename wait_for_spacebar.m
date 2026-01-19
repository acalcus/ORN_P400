function wait_for_spacebar()
%WAIT_FOR_SPACEBAR holds until spacebar is pressed
spaceKey = KbName('space');
while true
    [~,~, keyCode] = KbCheck;
    if (keyCode(spaceKey))
        break
    end
end
end


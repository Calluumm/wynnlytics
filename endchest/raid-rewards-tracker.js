JsMacros.assertEvent(event, "Service");
//Made by MFLR5 and NHK, added here for complete-ness
class RewardChest {
  constructor(pos, raid) {
    this.pos = pos;
    this.raid = raid;
  }
}

const REWARD_CHESTS = [
  new RewardChest(PositionCommon.createBlockPos(10342, 41, 3111), "Nest of the Grootslangs"),
  new RewardChest(PositionCommon.createBlockPos(11005, 58, 2909), "Orphion's Nexus of Light"),
  new RewardChest(PositionCommon.createBlockPos(10817, 45, 3901), "The Canyon Colossus"),
  new RewardChest(PositionCommon.createBlockPos(24489, 8, -23878), "The Nameless Anomaly"),
];
const REWARD_CHEST_TITLE = "󏿪";
const REWARD_SLOTS_START = 27;
const REWARD_SLOTS_LAST = 53;
const PREMATURE_REOPEN_WINDOW = 2500;
const CSV_FILE_NAME = "raid_rewards_data.csv";

let rewardsLastClosed;

const openListener = JsMacros.on("OpenContainer", JavaWrapper.methodToJavaAsync(event => {
  const title = event.inventory.getContainerTitle();

  if (title === REWARD_CHEST_TITLE) {
    const e = JsMacros.waitForEvent("ContainerUpdate", JavaWrapper.methodToJavaAsync(updateEvent => {
      return updateEvent.inventory.getContainerTitle() === title;
    })).event;

    e.screen.setOnClose(JavaWrapper.methodToJava(() => {
      rewardsLastClosed = Date.now();
    }));

    let data = [
      determineRaid(),
      Date.now()
    ];

    if (rewardsLastClosed && (data[1] - rewardsLastClosed) <= PREMATURE_REOPEN_WINDOW) {
      return;
    }

    const SEARCH_DEPTH = Math.min(Chat.getHistory().getRecvLines().length, 50);
    let pulls = -1;

    for (let i = 0; i < SEARCH_DEPTH; i++) {
      const text = Chat.getHistory().getRecvLines()[i].getText().getString();

      if (text.endsWith("Reward Pulls") && text.includes("§7")) {
        pulls = text.split("§7")[3].replace(" Reward Pulls", "");

        break;
      }
    }

    data.push(pulls);
    
    for (let slot = REWARD_SLOTS_START; slot <= REWARD_SLOTS_LAST; slot++) {
      const item = e.inventory.getSlot(slot);
      const name = item.getName().getString();

      data.push(name.includes("Powder") ? cleanName(name) : name);
      data.push(item.getCount());
    }

    if (pulls > 0) {
      saveData(data.join(","));
    }
  }
}));


Chat.getCommandManager().createCommandBuilder("copyrewarddata")
  .executes(JavaWrapper.methodToJavaAsync(context => {
    if (FS.exists(CSV_FILE_NAME)) {
      Client.getMinecraft().field_1774.method_1455(FS.open(CSV_FILE_NAME).read());
    }
    Chat.log("Reward Data Copied to Clipboard");
  }))
  .register();

function determineRaid() {
  const pos = Player.getPlayer().getBlockPos();
  let closestChest;
  
  for (let chest of REWARD_CHESTS) {
    const distance = pos.distanceTo(chest.pos);

    if (!closestChest || distance < pos.distanceTo(closestChest.pos)) {
      closestChest = chest;
    }
  }

  return closestChest.raid;
}

function cleanName(itemName) {
  return itemName.replace(/[] /, "");
}

function saveData(row) {
  if (!FS.exists(CSV_FILE_NAME)) {
    initFile();
  }

  FS.open(CSV_FILE_NAME).append(row).append("\n");
}

function initFile() {
  FS.createFile("./", CSV_FILE_NAME);

  const handler = FS.open(CSV_FILE_NAME);

  handler.write(`Raid,Timestamp,Total Pulls,Pull 1 Item,Pull 1 Count,Pull 2 Item,Pull 2 Count,Pull 3 Item,Pull 3 Count,Pull 4 Item,Pull 4 Count,Pull 5 Item,Pull 5 Count,Pull 6 Item,Pull 6 Count,Pull 7 Item,Pull 7 Count,Pull 8 Item,Pull 8 Count,Pull 9 Item,Pull 9 Count,Pull 10 Item,Pull 10 Count,Pull 11 Item,Pull 11 Count,Pull 12 Item,Pull 12 Count,Pull 13 Item,Pull 13 Count,Pull 14 Item,Pull 14 Count,Pull 15 Item,Pull 15 Count,Pull 16 Item,Pull 16 Count,Pull 17 Item,Pull 17 Count,Pull 18 Item,Pull 18 Count,Pull 19 Item,Pull 19 Count,Pull 20 Item,Pull 20 Count,Pull 21 Item,Pull 21 Count,Pull 22 Item,Pull 22 Count,Pull 23 Item,Pull 23 Count,Pull 24 Item,Pull 24 Count,Pull 25 Item,Pull 25 Count,Pull 26 Item,Pull 26 Count,Pull 27 Item,Pull 27 Count\n`);
}

event.stopListener = JavaWrapper.methodToJava(() => {
  JsMacros.off(openListener);
  Chat.getCommandManager().unregisterCommand("copyrewarddata");
});
